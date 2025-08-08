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
(define ((== u v) s/c)
  (match-define (state s c) s/c)
  (state (unify u v s) c))


(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj g ...)]
    [(_ (x x* ...) g ...) (call/fresh (λ (x) (fresh (x* ...) g ...)))]))


(define-syntax-rule (conde c ...) (cond-aux disje c ...))
(define-syntax-rule (condi c ...) (cond-aux disji c ...))

(define-syntax-rule (ife g0 g1 g2) (conde [g0 g1] [else g2]))
(define-syntax-rule (ifi g0 g1 g2) (condi [g0 g1] [else g2]))


(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real n^] [g (fresh (x) g* ...)])
    (for/list : (Listof Term)
              ([t (in-amb (walk (var 0) (state-substitution (g empty-s))))]
               [_ (in-range n)])
      t)))
(define-syntax-rule (run* (x) g* ...) (run +inf.0 (x) g* ...))
