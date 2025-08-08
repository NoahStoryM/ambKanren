#lang typed/racket/base

(require "private/ambKanren.rkt"
         "private/types.rkt"
         racket/match
         typed/amb)

(provide fail succeed
         ==
         fresh
         conde ife
         condi ifi
         run run*
         (rename-out [== ≡] [run* run∞])
         (all-from-out "private/types.rkt"))


(: == (→ Term Term Goal))
(define ((== v w) s/c)
  (match-define (state s c) s/c)
  (state (unify v w s) c))


(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj g ...)]
    [(_ (x x* ...) g ...) (call/fresh (λ (x) (fresh (x* ...) g ...)))]))


(define-syntax-rule (conde c ...) (cond-aux disje c ...))
(define-syntax-rule (condi c ...) (cond-aux disji c ...))

(define-syntax-rule (ife g0 g1 g2) (conde [g0 g1] [else g2]))
(define-syntax-rule (ifi g0 g1 g2) (condi [g0 g1] [else g2]))


(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real n^]
        [x (var 0)]
        [g (fresh (x) g* ...)]
        [s/c (state empty-s 0)])
    (for/list : (Listof Term)
              ([s/c (in-amb (g s/c))]
               [_ (in-range n)])
      (match-define (state s c) s/c)
      (reify (walk x s)))))
(define-syntax-rule (run* (x) g* ...) (run +inf.0 (x) g* ...))
