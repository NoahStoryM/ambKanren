#lang typed/racket/base

(provide (all-defined-out))

(define-type Term
  (∪ Boolean Complex Char Bytes String Keyword Null Symbol
     Var (Pair Term Term)))

(struct var
  ([counter : Natural])
  #:type-name Var
  #:transparent)
(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (= (var-counter x1) (var-counter x2)))

(define-type Substitution (Immutable-HashTable Var Term))
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

(struct state
  ([substitution : Substitution]
   [counter : Natural])
  #:type-name State
  #:transparent)

(define-type Goal (→ State State))
(: apply-goal (→ Goal State State))
(define (apply-goal g s/c) (g s/c))
