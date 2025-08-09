#lang typed/racket/base

(require "private/types.rkt"
         "mini.rkt"
         racket/match
         typed/amb)

(provide (all-from-out "private/types.rkt")
         fail succeed
         ==
         fresh
         conde ife
         run run*
         (rename-out [== ≡] [run* run∞]))


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


(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real (or n^ +inf.0)]
        [x (var 0)]
        [g (fresh (x) g* ...)]
        [s/c (state empty-s 0)])
    (for/list : (Listof Term)
              ([s/c (in-amb (g s/c))]
               [_ (in-range n)])
      (match-define (state s c) s/c)
      (reify (walk x s)))))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))
