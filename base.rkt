#lang typed/racket/base

(require "private/types.rkt"
         "micro.rkt"
         (except-in racket/match ==))

(provide (all-from-out "private/types.rkt")
         fail succeed
         == (rename-out [== ≡])
         fresh
         cond-aux conde ife)


(: == (→ Term Term Goal))
(define ((== v w) s/c)
  (match-define (state s c) s/c)
  (state (unify v w s) c))


(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj g ...)]
    [(_ (x x* ...) g ...) (call/fresh (λ (x) (fresh (x* ...) g ...)))]))


(define-syntax cond-aux
  (syntax-rules (else)
    [(_ disj) (disj)]
    [(_ disj [else g ...]) (conj g ...)]
    [(_ disj [g ...]) (conj g ...)]
    [(_ disj [g ...] c ...)
     (disj (conj g ...) (cond-aux disj c ...))]))
(define-syntax-rule (conde c ...) (cond-aux disje c ...))
(define-syntax-rule (ife g0 g1 g2) (conde [g0 g1] [else g2]))
