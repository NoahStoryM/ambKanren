#lang racket/base

;; A small untyped TRS2-style miniKanren over amb, with the search
;; combinators swappable at run time, so that one set of example
;; programs can be run under several scheduling schemes.
;;
;;   Goal : Substitution -> Substitution          (direct style)
;;
;; A *scheme* supplies disj2, conj2 and `pause` (the suspension that
;; TRS2's defrel inserts), plus a top-level runner.  Schemes are
;; defined in schemes.rkt.  Terms/walk/unify follow
;; miniKanren/amb/trs2/main.rkt (which follows trs2-impl.scm).

(require amb (for-syntax racket/base))
(provide (struct-out var) (struct-out scheme)
         empty-s walk walk* unify reify-term
         == succeed fail disj2 conj2 pause! use-scheme! current-scheme-name
         disj conj conde fresh defrel run run*
         ;; instrumentation
         reset-counters! counter bump! trace! trace-on? with-trace
         fuel exn:starved? exn:starved-pauses)

;;; ---- instrumentation ------------------------------------------------

(define counters (make-hasheq))
(define (reset-counters!) (hash-clear! counters))
(define (counter k) (hash-ref counters k 0))

;; `fuel`: abort the whole run once this many pauses have happened,
;; so that starvation is a reported result, not a hang.
(define fuel (make-parameter 100000))
(struct exn:starved exn:fail (pauses))
(define (bump! k)
  (define n (add1 (counter k)))
  (hash-set! counters k n)
  (when (and (eq? k 'pause) (> n (fuel)))
    (raise (exn:starved (format "starved: ~a pauses" n)
                        (current-continuation-marks) n))))

(define trace-on? (make-parameter #f))
(define trace-log '())
(define (trace! . ev)
  (when (trace-on?) (set! trace-log (cons ev trace-log))))
(define (with-trace thk)
  (set! trace-log '())
  (define r (parameterize ([trace-on? #t]) (thk)))
  (values r (reverse trace-log)))

;;; ---- terms ----------------------------------------------------------

(struct var (name) #:authentic)
(define empty-s #hasheq())

(define (walk v s)
  (if (and (var? v) (hash-has-key? s v)) (walk (hash-ref s v) s) v))

(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond [(var? v) (eq? v x)]
          [(pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s))]
          [else #f])))

(define (ext-s x v s) (if (occurs? x v s) (amb) (hash-set s x v)))

(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond [(eq? u v) s]
          [(var? u) (ext-s u v s)]
          [(var? v) (ext-s v u s)]
          [(and (pair? u) (pair? v))
           (unify (cdr u) (cdr v) (unify (car u) (car v) s))]
          [(equal? u v) s]
          [else (amb)])))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond [(var? v) v]
          [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
          [else v])))

(define (reify-s v r)
  (let ([v (walk v r)])
    (cond [(var? v)
           (hash-set r v (string->symbol (format "_~a" (hash-count r))))]
          [(pair? v) (reify-s (cdr v) (reify-s (car v) r))]
          [else r])))

(define ((reify-term v) s)
  (let* ([v (walk* v s)] [r (reify-s v empty-s)]) (walk* v r)))

;;; ---- scheme plumbing ------------------------------------------------

;; disj2 conj2 : Goal Goal -> Goal ; pause : -> Void ;
;; run-goal : (Option Natural) Goal -> (Listof Substitution)
(struct scheme (name disj2 conj2 pause run-goal))
(define current #f)
(define (use-scheme! sc) (set! current sc))
(define (current-scheme-name) (scheme-name current))

(define ((== u v) s) (unify u v s))
(define (succeed s) s)
(define (fail s) (amb))
(define (disj2 g1 g2) ((scheme-disj2 current) g1 g2))
(define (conj2 g1 g2) ((scheme-conj2 current) g1 g2))
(define (pause!) (bump! 'pause) (trace! 'suspend) ((scheme-pause current)))

(define-syntax disj
  (syntax-rules ()
    [(_) fail] [(_ g) g] [(_ g0 g ...) (disj2 g0 (disj g ...))]))
(define-syntax conj
  (syntax-rules ()
    [(_) succeed] [(_ g) g] [(_ g0 g ...) (conj2 g0 (conj g ...))]))
(define-syntax-rule (conde [g ...] ...) (disj (conj g ...) ...))
(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj g ...)]
    [(_ (x0 x ...) g ...)
     (let ([x0 (var 'x0)]) (fresh (x ...) g ...))]))
;; TRS2: (define (name arg ...) (λ (s) (λ () ((conj g ...) s))))
;; `[x : T]` parameters are accepted (and the type ignored) so the same
;; program text also runs under Typed Racket.
(define-syntax (defrel stx)
  (syntax-case stx ()
    [(_ (name arg ...) g ...)
     (with-syntax ([(x ...) (for/list ([a (syntax->list #'(arg ...))])
                              (syntax-case a () [(x _ t) #'x] [x #'x]))])
       #'(define ((name x ...) s)
           (pause!)
           ((conj g ...) s)))]))

(define-syntax run
  (syntax-rules ()
    [(_ n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...) (== (list x0 x ...) q) g ...))]
    [(_ n q g ...)
     (let ([q (var 'q)])
       (map (reify-term q)
            ((scheme-run-goal current) n (conj g ...))))]))
(define-syntax-rule (run* q g ...) (run #f q g ...))
