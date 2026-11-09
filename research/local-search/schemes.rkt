#lang racket/base

;; Scheduling schemes for kernel.rkt.  All keep Goal : S -> S.
;;
;;   global        ambKanren trs2 today: disj2 = ((amb g1 g2) s),
;;                 conj2 = composition, suspension = amb frontier trick.
;;   answers       Experiment B: disj2 is a LOCAL generator that drives
;;                 (g1 s) and (g2 s) in two nested in-amb/do searches and
;;                 alternates their ANSWERS, reflected with make-amb.
;;                 Suspension as in `global` (invisible to disj2).
;;   pause/disj    Experiment D: a suspension is an observable PAUSE leaf
;;                 of the nearest local search (extra prompt tag); disj2
;;                 = reflect (append-inf (reify g1 s) (reify g2 s));
;;                 conj2 = composition.
;;   pause/full    as pause/disj, and conj2 = reflect (append-map-inf ..).
;;
;; The pause protocol needs NO change to amb: a local search is
;; `in-amb/do` + a pause prompt; `pause!` = (amb <escape-with-PAUSE> <continue>).

(require racket/control amb "kernel.rkt")
(provide global answers pause/disj pause/full
         ;; protocol pieces, for traces and direct experiments
         reify gen->inf reflect append-inf append-map-inf take-inf
         PAUSE DONE)

;;; ---- shared: ambKanren trs2's suspension and runner -----------------

(define (amb-suspend)
  (parameterize ([current-amb-depth-first? #f]) (amb (void)))
  (amb (void)))

(define (amb-run-goal n g)
  (parameterize ([current-amb-depth-first? #t] [current-amb-fair? #f])
    (for/list ([_ (in-range (or n +inf.0))]
               [s (in-amb/do (g empty-s))])
      s)))

(define (compose-conj2 g1 g2) (λ (s) (g2 (g1 s))))

(define global
  (scheme 'global
          (λ (g1 g2) (λ (s) ((amb g1 g2) s)))
          compose-conj2
          amb-suspend
          amb-run-goal))

;;; ---- Experiment B: answer-level local disj --------------------------

(define scope-id 0)
(define (new-scope! label)
  (set! scope-id (add1 scope-id))
  (bump! 'spawn)
  (trace! 'spawn scope-id label)
  scope-id)

;; A lazily created generator over the answers of (g s).
(define (answer-gen g s label)
  (define id (new-scope! label))
  (define-values (more? get) (sequence-generate (in-amb/do (g s))))
  (cons (λ () (begin0 (more?) (void)))
        (λ () (define v (get)) (trace! 'yield id) v)))

(define (answers-disj2 g1 g2)
  (λ (s)
    (define gens (vector #f #f))
    (define live (vector #t #t))
    (define turn 0)
    (define ready #f)
    (define (gen i)
      (or (vector-ref gens i)
          (let ([gn (answer-gen (if (= i 0) g1 g2) s (if (= i 0) 'left 'right))])
            (vector-set! gens i gn)
            gn)))
    (define (more?)
      (or (and ready #t)
          (let loop ([tries 0])
            (cond
              [(= tries 2) #f]
              [(not (vector-ref live turn))
               (set! turn (- 1 turn)) (loop (add1 tries))]
              [((car (gen turn))) (set! ready turn) #t]
              [else (vector-set! live turn #f)
                    (set! turn (- 1 turn)) (loop (add1 tries))]))))
    (define (get)
      (define i ready)
      (set! ready #f)
      (set! turn (- 1 i))
      ((cdr (gen i))))
    (make-amb more? get)))

(define answers
  (scheme 'answers answers-disj2 compose-conj2 amb-suspend amb-run-goal))

;;; ---- Experiment D: observable pauses --------------------------------

(define pause-tag (make-continuation-prompt-tag 'kanren-pause))
(define PAUSE (string->uninterned-symbol "PAUSE"))
(define DONE  (string->uninterned-symbol "DONE"))

;; Keep the rest of the computation as the second alternative of a
;; choice point and report PAUSE, as a leaf, to the nearest local
;; search.  With amb's default strategy (df #t, fair #f) that choice
;; point is the front of the local frontier, so the next step of the
;; local search resumes exactly this computation.
(define (protocol-pause)
  (when (amb #t #f)
    (abort-current-continuation pause-tag)))

;; A local search over (g s): a one-shot step function returning the
;; next event: an answer, PAUSE or DONE.  Nothing runs until the first
;; step.
(define (reify g s [label 'reify])
  (define id (new-scope! label))
  (define-values (more? get)
    (sequence-generate
     (in-amb/do
      (call-with-continuation-prompt (λ () (g s)) pause-tag (λ () PAUSE)))))
  (λ ()
    (cond [(more?) (define v (get))
                   (trace! (if (eq? v PAUSE) 'pause 'yield) id)
                   v]
          [else (trace! 'done id) DONE])))

;; TRS2 stream from a step function: the mature prefix is computed
;; eagerly, a PAUSE becomes a suspension (a thunk), exactly the shape
;; (g s) has in trs2-impl.scm.  One-shot: TRS2's combinators force each
;; suspension at most once, which is all this needs.
(define (gen->inf step)
  (let loop ()
    (define v (step))
    (cond [(eq? v DONE) '()]
          [(eq? v PAUSE) (λ () (loop))]
          [else (cons v (loop))])))

;; trs2-impl.scm, verbatim modulo names.
(define (append-inf s-inf t-inf)
  (cond [(null? s-inf) t-inf]
        [(pair? s-inf) (cons (car s-inf) (append-inf (cdr s-inf) t-inf))]
        [else (λ () (append-inf t-inf (s-inf)))]))
(define (append-map-inf g s-inf)
  (cond [(null? s-inf) '()]
        [(pair? s-inf) (append-inf (g (car s-inf)) (append-map-inf g (cdr s-inf)))]
        [else (λ () (append-map-inf g (s-inf)))]))
(define (take-inf n s-inf)
  (cond [(and n (zero? n)) '()]
        [(null? s-inf) '()]
        [(pair? s-inf) (cons (car s-inf) (take-inf (and n (sub1 n)) (cdr s-inf)))]
        [else (take-inf n (s-inf))]))

;; Back to direct style: answers become choice points, suspensions are
;; re-emitted as pauses of the enclosing local search.
(define (reflect inf)
  (cond [(null? inf) (amb)]
        [(pair? inf) (amb (car inf) (reflect (cdr inf)))]
        [else (bump! 'relay) (protocol-pause) (reflect (inf))]))

(define (pause-disj2 g1 g2)
  (λ (s)
    (reflect (append-inf (gen->inf (reify g1 s 'disj-left))
                         (gen->inf (reify g2 s 'disj-right))))))

(define (pause-conj2 g1 g2)
  (λ (s)
    (reflect (append-map-inf (λ (s1) (gen->inf (reify g2 s1 'conj-right)))
                             (gen->inf (reify g1 s 'conj-left))))))

(define (pause-run-goal n g)
  (parameterize ([current-amb-depth-first? #t] [current-amb-fair? #f])
    (take-inf n (gen->inf (reify g empty-s 'run)))))

(define pause/disj
  (scheme 'pause/disj pause-disj2 compose-conj2 protocol-pause pause-run-goal))
(define pause/full
  (scheme 'pause/full pause-disj2 pause-conj2 protocol-pause pause-run-goal))
