;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper
#lang typed/racket/base

(require "nano.rkt"
         racket/match
         typed/amb)

(provide (all-from-out "nano.rkt")
         (all-defined-out))


(: walk (→ Term Substitution Term))
(define (walk v s)
  (if (and (var? v) (in-s? v s))
      (walk (apply-s s v) s)
      v))


(: == (→ Term Term Goal))
(define ((== v w) s/c)
  (match-define (state s c) s/c)
  (state (unify v w s) c))



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


(: fail Goal)
(: succeed Goal)
(define (fail _) (amb))
(define (succeed s/c) (amb s/c))


(: disj (→ Goal * Goal))
(define (disj . g*)
  (match (remq* (list fail) g*)
    ['() fail]
    [`(,g) g]
    [g* (λ (s/c) ((sequence->amb g*) s/c))]))

(: conj (→ Goal * Goal))
(define (conj . g*)
  (if (memq fail g*)
      fail
      (match (remq* (list succeed) g*)
        ['() succeed]
        [`(,g) g]
        [g* (λ (s/c) (foldl apply-goal s/c g*))])))


(: call/fresh (→ (→ Var Goal) Goal))
(define ((call/fresh f) s/c)
  (match-define (state s c) s/c)
  (define g (f (var c)))
  (g (state s (add1 c))))
