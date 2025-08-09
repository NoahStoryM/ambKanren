#lang typed/racket/base

(provide (all-defined-out))

(define-type Term
  (∪ Boolean Number Char Bytes String Keyword Null Symbol
     Var (Pair Term Term)))

(struct var
  ([counter : Natural])
  #:type-name Var
  #:transparent)

(define-type Substitution (Immutable-HashTable Var Term))

(struct state
  ([substitution : Substitution]
   [counter : Natural])
  #:type-name State
  #:transparent)

(define-type Goal (→ State State))
