#lang typed/racket/base

(require "micro.rkt"
         (except-in racket/match ==))

(provide (all-from-out "micro.rkt")
         fail succeed
         ==
         Zzz
         fresh
         conj+ disj+
         all-aux all
         cond-aux conde ife
         (rename-out
          [succeed ⊤]
          [fail ⊥]
          [== ≡]
          [fresh ∃]
          [disj+ ∨]
          [conj+ ∧]))


(define-syntax-rule (Zzz g) (ann (λ (s/c) (g s/c)) Goal))

(define-syntax-rule (disj+ g ...) (disj (Zzz g) ...))
(define-syntax-rule (conj+ g ...) (conj (Zzz g) ...))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x0 x ...) g ...)
     (call/fresh
      (λ (x0)
        (fresh (x ...) g ...)))]))

(define-syntax all-aux
  (syntax-rules ()
    [(_ conj+) succeed]
    [(_ conj+ g) (Zzz g)]
    [(_ conj+ g ...) (conj+ g ...)]))
(define-syntax-rule (all g ...) (all-aux conj+ g ...))

(define-syntax cond-aux
  (syntax-rules (else)
    [(_ disj+) fail]
    [(_ disj+ [else g ...]) (all g ...)]
    [(_ disj+ [g ...]) (all g ...)]
    [(_ disj+ [g ...] c ...)
     (disj+ (all g ...) (cond-aux disj+ c ...))]))
(define-syntax-rule (conde c ...) (cond-aux disj+ c ...))
(define-syntax-rule (ife g0 g1 g2) (conde [g0 g1] [else g2]))
