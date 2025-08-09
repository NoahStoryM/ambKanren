;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper
#lang typed/racket/base

(require "nano.rkt"
         racket/match
         typed/amb)

(provide (all-from-out "nano.rkt")
         (all-defined-out))


(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (= (var-counter x1) (var-counter x2)))


(: walk (→ Term Substitution Term))
(define (walk v s)
  (if (and (var? v) (in-s? v s))
      (walk (apply-s s v) s)
      v))


(: empty-s Substitution)
(: ext-s (→ Var Term Substitution Substitution))
(: size-s (→ Substitution Index))
(: in-s? (→ Var Substitution Boolean))
(: apply-s (→ Substitution Var Term))
(define empty-s #hash())
(define (ext-s x v s) (hash-set s x v))
(define (size-s s) (hash-count s))
(define (in-s? v s) (hash-has-key? s v))
(define (apply-s s v) (hash-ref s v))


(: == (→ Term Term Goal))
(define ((== v w) s/c)
  (match-define (state s c) s/c)
  (state (unify v w s) c))



(: unify (→ Term Term Substitution Substitution))
(define (unify v w s)
  (let ([v (walk v s)] [w (walk w s)])
    (cond
      [(and (var? v) (var? w) (var=? v w)) s]
      [(var? v) (ext-s v w s)]
      [(var? w) (ext-s w v s)]
      [(and (pair? v) (pair? w))
       (unify (cdr v) (cdr w) (unify (car v) (car w) s))]
      [(equal? v w) s]
      [else (amb)])))


(: fail Goal)
(: succeed Goal)
(define (fail _) (amb))
(define (succeed s/c) (amb s/c))


(: disje (→ Goal * Goal))
(define (disje . g*)
  (match (remq* (list fail) g*)
    ['() fail]
    [`(,g) g]
    [g*
     (λ (s/c)
       ((for/amb : Goal
                 #:length (length g*)
                 #:fill fail
                 ([g (in-list g*)])
          g)
        s/c))]))

(: conj (→ Goal * Goal))
(define (conj . g*)
  (match (remq* (list succeed) g*)
    ['() succeed]
    [`(,g) g]
    [g* (λ (s/c) (for/fold ([s/c s/c]) ([g (in-list g*)]) (g s/c)))]))


(: call/fresh (→ (→ Var Goal) Goal))
(define ((call/fresh f) s/c)
  (match-define (state s c) s/c)
  (define g (f (var c)))
  (g (state s (add1 c))))
