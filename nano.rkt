#lang typed/racket/base

(require "types.rkt"
         typed/amb)

(provide (all-from-out "types.rkt")
         (all-defined-out))


(: list->amb (∀ (a) (→ (Listof a) a)))
(define (list->amb a*)
  (for/amb : a #:length (length a*) #:fill (amb) ([a (in-list a*)]) a))


(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (equal? x1 x2))


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


(: apply-goal (→ Goal State State))
(define (apply-goal g s/c) (g s/c))
