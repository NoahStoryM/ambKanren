#lang typed/racket/base

(require "private/types.rkt"
         (except-in racket/match ==)
         typed/racket/unsafe
         typed/data/queue
         typed/amb)

(provide (all-from-out "private/types.rkt")
         (all-defined-out))


(: sequence->amb (∀ (a) (→ (Sequenceof a) a)))
(: list->amb (∀ (a) (→ (Listof a) a)))
(define (sequence->amb a*) (for/amb : a ([a a*]) a))
(define (list->amb a*) (for/amb : a #:length (length a*) ([a a*]) a))

(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (eq? x1 x2))

(: empty-s Substitution)
(: s-empty? (→ Substitution Boolean))
(: s-has-key? (→ Var Substitution Boolean))
(: apply-s (→ Substitution Var Term))
(: size-s (→ Substitution Index))
(: ext-s (→ Var Term Substitution Substitution))
(define empty-s #hasheq())
(define (s-empty? s) (hash-empty? s))
(define (s-has-key? v s) (hash-has-key? s v))
(define (apply-s s v) (hash-ref s v))
(define (size-s s) (hash-count s))
(define (ext-s x v s)
  (when (occurs? x v s) (amb))
  (hash-set s x v))

(: apply-goal (→ Goal Substitution Substitution))
(define (apply-goal g s) (g s))


(unsafe-require/typed "private/utils.rkt"
  [rotate-queue! (→ SequenceTop Void)])
(define rotate-tasks! rotate-queue!)
(define make-tasks (current-amb-maker))
(define tasks-length (current-amb-length))
(define tasks-add! (current-amb-pusher))
(define tasks-del! (current-amb-popper))


(: walk (→ Term Substitution Term))
(define (walk v s)
  (if (and (var? v) (s-has-key? v s))
      (walk (apply-s s v) s)
      v))

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (match (walk v s)
    [(cons v0 v1)
     (cons (walk* v0 s)
           (walk* v1 s))]
    [v v]))


(: == (→ Term Term Goal))
(define ((== v w) s) (unify v w s))

(: unify (→ Term Term Substitution Substitution))
(define (unify v w s)
  (match* ((walk v s) (walk w s))
    [((? var? v) (? var? w)) #:when (var=? v w) s]
    [((? var? v) w) (ext-s v w s)]
    [(v (? var? w)) (ext-s w v s)]
    [((cons v0 v1) (cons w0 w1))
     (unify v1 w1 (unify v0 w0 s))]
    [(v w) #:when (equal? v w) s]
    [(_ _) (amb)]))


(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (or (and (var? v)
             (var=? x v))
        (and (pair? v)
             (or (occurs? x (car v) s)
                 (occurs? x (cdr v) s))))))


(: reify-name (→ Index Symbol))
(define (reify-name n)
  (string->symbol (format "_.~a" n)))

(: reify-s (→ Term Substitution Substitution))
(define (reify-s v s)
  (match (walk v s)
    [(? var? v) (ext-s v (reify-name (size-s s)) s)]
    [(cons v0 v1) (reify-s v1 (reify-s v0 s))]
    [_ s]))

(: reify (→ Term Term))
(define (reify v) (walk* v (reify-s v empty-s)))


(: fail Goal)
(: succeed Goal)
(define (fail _) (amb))
(define (succeed s) (amb s))

(: disj (→ Goal * Goal))
(define (disj . g*)
  (match (remq* (list fail) g*)
    ['() fail]
    [`(,g) g]
    [`(,g1 ,g2) (λ (s) ((amb g1 g2) s))]
    [g* (λ (s) ((list->amb g*) s))]))

(: conj (→ Goal * Goal))
(define (conj . g*)
  (if (memq fail g*)
      fail
      (match (remq* (list succeed) g*)
        ['() succeed]
        [`(,g) g]
        [`(,g1 ,g2) (λ (s) (g2 (g1 s)))]
        [g* (λ (s) (foldl apply-goal s g*))])))

(: mplusi (→ (Sequenceof Substitution) * (Sequenceof Substitution)))
(define (mplusi . s**)
  (: q (Queue (Sequenceof Substitution) (Sequenceof Substitution)))
  (define q (make-queue))
  (for ([s* (in-list s**)])
    (enqueue! q s*))
  (define fail-s : Substitution #hash())
  (: producer (→ Substitution))
  (define (producer)
    (if (queue-empty? q)
        fail-s
        (let* ([s* (dequeue! q)]
               [s (for/or : (Option Substitution) ([s : Substitution s*]) s)])
          (cond
            [s (enqueue! q s*) s]
            [else (producer)]))))
  (in-producer producer (λ (s) (eq? s fail-s))))

(: disji (→ Goal * Goal))
(define (disji . g*)
  (match (remq* (list fail) g*)
    ['() fail]
    [`(,g) g]
    [`(,g1 ,g2)
     (λ (s)
       (sequence->amb
        (mplusi (in-amb/do (g1 s))
                (in-amb/do (g2 s)))))]
    [g*
     (λ (s)
       (define s**
         (for/list : (Listof (Sequenceof Substitution))
                   ([g (in-list g*)])
           (in-amb/do (g s))))
       (sequence->amb (apply mplusi s**)))]))

(: conji (→ Goal * Goal))
(define (conji . g*)
  (if (memq fail g*)
      fail
      (match (remq* (list succeed) g*)
        ['() succeed]
        [`(,g) g]
        [g*
         (λ (s)
           (define s*
             (for/fold : (Sequenceof Substitution)
                       ([s* (in-value s)])
                       ([g (in-list g*)])
               (in-amb/do
                (let* ([s (sequence->amb s*)]
                       [s* (in-amb/do (g s))])
                  (parameterize ([current-amb-rotator rotate-tasks!])
                    (sequence->amb s*))))))
           (sequence->amb s*))])))
