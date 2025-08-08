#lang typed/racket/base

(provide (all-defined-out))

(define-type Term (∪ Boolean Complex Char Bytes String Keyword Null Symbol Var (Pair Term Term)))
(define-type Substitution (Immutable-HashTable Var Term))
(define-type Goal (→ State State))

(struct var
  ([counter : Natural])
  #:type-name Var
  #:transparent)

(struct state
  ([substitution : Substitution]
   [counter : Natural])
  #:type-name State
  #:transparent)
