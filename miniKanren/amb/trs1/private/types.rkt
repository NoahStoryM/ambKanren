#lang typed/racket/base

(provide (all-defined-out))

(define-type Literal (∪ Boolean Number Char Bytes String Keyword Null Symbol))
(define-type Data (∪ Literal (Pair Data Data)))
(define-type Term (∪ Literal Var (Pair Term Term)))
(define-type Substitution (Immutable-HashTable Var Term))
(define-type Goal (→ Substitution Substitution))

(struct var ([name : Symbol])
  #:type-name Var
  #:transparent)

(define-predicate data? Data)
