#lang typed/racket/base

(provide (all-defined-out))

(define-type Term
  (∪ Boolean Number Char Bytes String Keyword Null Symbol
     Var (Pair Term Term)))
(define-type Substitution (Immutable-HashTable Var Term))
(define-type Goal (→ Substitution Substitution))

(struct var ([name : Symbol])
  #:type-name Var
  #:transparent)
