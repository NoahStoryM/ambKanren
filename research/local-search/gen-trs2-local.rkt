#lang racket/base
;; Splice the local-search protocol into a copy of ambKanren trs2/main.rkt.
;; Usage: cp ../../miniKanren/amb/trs2/main.rkt trs2-local.rkt && racket gen-trs2-local.rkt trs2-local.rkt
;;        racket gen-fused.rkt trs2-local.rkt trs2-local-fused.rkt
(require racket/file racket/string)
(define target (vector-ref (current-command-line-arguments) 0))
(define lines (file->lines target))
(define (slice a b) (for/list ([l lines] [i (in-naturals 1)] #:when (<= a i b)) l))
(define (find rx) (for/first ([l lines] [i (in-naturals 1)] #:when (regexp-match? rx l)) i))

(define header-new #<<EOF
#lang typed/racket/base

;; RESEARCH PROTOTYPE (ambKanren issue #14).  TRS2 miniKanren that keeps
;; Goal : Substitution -> Substitution, but whose search combinators are
;; the stream combinators of trs2-impl.scm applied LOCALLY:
;;
;;   reify-goal : Goal Substitution -> Inf   a nested in-amb/do search
;;                of (g s) in which a suspension is an observable PAUSE
;;                leaf (one extra prompt tag);
;;   reflect    : Inf -> Substitution        answers become amb choice
;;                points, suspensions are re-emitted as pauses.
;;
;; disj2 / conj2 / ifte / once are literally append-inf / append-map-inf
;; / ifte / once of the second edition, lifted by
;;   (lift f) = (λ (s) (reflect (f s))).
;; No change to amb is required.  Everything else is unchanged from
;; miniKanren/amb/trs2/main.rkt, whose header follows.
;;
;; ambKanren with the language of The Reasoned Schemer, second edition.
EOF
  )

(define protocol #<<EOF

;;; Local search protocol

;; A TRS2 stream; built lazily from a local search, consumed once.
(define-type Inf (Rec I (U Null (Pairof Substitution I) (→ I))))

(: pause-tag (Prompt-Tagof (U Substitution Symbol) (→ Symbol)))
(define pause-tag (make-continuation-prompt-tag 'kanren-pause))
(define PAUSE : Symbol (string->uninterned-symbol "PAUSE"))

;; The suspension of an immature stream, made observable: keep the rest
;; of the computation as the second alternative of a choice point, and
;; report PAUSE as a leaf of the nearest local search.  With amb's
;; default strategy (depth-first, unfair; fixed in run-goal) that choice
;; point is resumed at the next step of the same local search.
(: suspend (→ Void))
(define (suspend)
  (when (amb #t #f)
    (abort-current-continuation pause-tag)))

;; reify: run (g s) as a local search and present it as a TRS2 stream.
;; The mature prefix is computed eagerly and a PAUSE becomes a thunk,
;; which is the shape (g s) has in trs2-impl.scm.
(: reify-goal (→ Goal Substitution Inf))
(define (reify-goal g s)
  (define-values (more? get)
    (sequence-generate
     (in-amb/do
      (call-with-continuation-prompt
       (λ () : (U Substitution Symbol) (g s))
       pause-tag
       (λ () PAUSE)))))
  (let loop : Inf ()
    (if (more?)
        (let ([v (get)])
          (if (symbol? v) (λ () (loop)) (cons v (loop))))
        '())))

;; Two-way choice point; its `info` names the combinator.
(: choose2 (→ Substitution (→ Substitution) Substitution))
(define (choose2 a thk)
  (define n 0)
  ((inst make-amb Substitution)
   (λ () (< n 2))
   (λ () (set! n (add1 n)) (if (= n 1) a (thk)))
   'reflect))

;; reflect: back to direct style.
(: reflect (→ Inf Substitution))
(define (reflect s-inf)
  (cond
    [(null? s-inf) (amb)]
    [(pair? s-inf) (choose2 (car s-inf) (λ () (reflect (cdr s-inf))))]
    [else (suspend) (reflect (s-inf))]))

(: lift (→ (→ Substitution Inf) Goal))
(define ((lift f) s) (reflect (f s)))

(: append-inf (→ Inf Inf Inf))
(define (append-inf s-inf t-inf)
  (cond
    [(null? s-inf) t-inf]
    [(pair? s-inf) (cons (car s-inf) (append-inf (cdr s-inf) t-inf))]
    [else (λ () (append-inf t-inf (s-inf)))]))

(: append-map-inf (→ (→ Substitution Inf) Inf Inf))
(define (append-map-inf g s-inf)
  (cond
    [(null? s-inf) '()]
    [(pair? s-inf) (append-inf (g (car s-inf)) (append-map-inf g (cdr s-inf)))]
    [else (λ () (append-map-inf g (s-inf)))]))

(: take-inf (→ (Option Natural) Inf (Listof Substitution)))
(define (take-inf n s-inf)
  (cond
    [(and n (zero? n)) '()]
    [(null? s-inf) '()]
    [(pair? s-inf) (cons (car s-inf) (take-inf (and n (sub1 n)) (cdr s-inf)))]
    [else (take-inf n (s-inf))]))

(: disj2 (→ Goal Goal Goal))
(define (disj2 g1 g2)
  (lift (λ (s) (append-inf (reify-goal g1 s) (reify-goal g2 s)))))

(: conj2 (→ Goal Goal Goal))
(define (conj2 g1 g2)
  (lift (λ (s) (append-map-inf (λ ([s : Substitution]) (reify-goal g2 s))
                               (reify-goal g1 s)))))

(: call/fresh (→ Symbol (→ Var Goal) Goal))
(define (call/fresh name f) (f (var name)))

(: ifte (→ Goal Goal Goal Goal))
(define (ifte g1 g2 g3)
  (lift
   (λ (s)
     (let loop : Inf ([s-inf (reify-goal g1 s)])
       (cond
         [(null? s-inf) (reify-goal g3 s)]
         [(pair? s-inf)
          (append-map-inf (λ ([s : Substitution]) (reify-goal g2 s)) s-inf)]
         [else (λ () (loop (s-inf)))])))))

(: once (→ Goal Goal))
(define (once g)
  (lift
   (λ (s)
     (let loop : Inf ([s-inf (reify-goal g s)])
       (cond
         [(null? s-inf) '()]
         [(pair? s-inf) (list (car s-inf))]
         [else (λ () (loop (s-inf)))])))))

EOF
  )

(define runner #<<EOF
(: run-goal (→ (Option Natural) Goal (Listof Substitution)))
(define (run-goal n g)
  (parameterize ([current-amb-depth-first? #t] [current-amb-fair? #f])
    (take-inf n (reify-goal g empty-s))))

EOF
  )

(define i-hdr  (find #rx"^;; ambKanren with the language"))
(define i-disj (find #rx"^;; mplus: a choice"))
(define i-reif (find #rx"^;;; Reification"))
(define i-run  (find #rx"^\\(: run-goal"))
(define i-syn  (find #rx"^;;; Syntax"))
(display-lines-to-file
 (append (list header-new)
         (slice (add1 i-hdr) (sub1 i-disj))
         (list protocol)
         (slice i-reif (sub1 i-run))
         (list runner)
         (slice i-syn (length lines)))
 target #:exists 'replace)
