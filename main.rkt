#lang typed/racket/base

(require "base.rkt"
         (except-in racket/match ==)
         typed/amb)

(provide (all-from-out "base.rkt")
         (all-defined-out)
         (rename-out
          [run* run∞]
          [succeed ⊤]
          [fail ⊥]
          [== ≡]
          [fresh ∃]
          [disj+ ∨]
          [conj+ ∧]
          [disji+ ∨i]
          [conji+ ∧i]))


(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real (or n^ +inf.0)]
        [x (var 'x)])
    (define g (all g* ...))
    (define s*
      (parameterize ([current-amb-rotator void]
                     [current-amb-shuffler void]
                     [current-amb-maker make-tasks]
                     [current-amb-length tasks-length]
                     [current-amb-pusher tasks-add!]
                     [current-amb-popper tasks-del!])
        (in-amb/do (g empty-s))))
    (for/list : (Listof Term)
              ([s : Substitution s*]
               [_ (in-range n)])
      (reify (walk* x s)))))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))

(define-syntax-rule (λG (x) body ...) (ann (λ (x) body ...) Goal))
(define-syntax-rule (Zzz g) (λG (s) (g s)))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x ...) g ...)
     (Zzz (let ([x (var 'x)] ...) (all g ...)))]))
(define-syntax project
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x ...) g ...)
     (λG (s) (let ([x (walk* x s)] ...) ((all g ...) s)))]))

(define-syntax-rule (disj+  g ...) (disj  (Zzz g) ...))
(define-syntax-rule (disji+ g ...) (disji (Zzz g) ...))
(define-syntax-rule (conj+  g ...) (conj  (Zzz g) ...))
(define-syntax-rule (conji+ g ...) (conji (Zzz g) ...))

(define-syntax all-aux
  (syntax-rules ()
    [(_ conj+) succeed]
    [(_ conj+ g) (Zzz g)]
    [(_ conj+ g ...) (conj+ g ...)]))
(define-syntax-rule (all  g ...) (all-aux conj+  g ...))
(define-syntax-rule (alli g ...) (all-aux conji+ g ...))

(define-syntax-rule (if-aux disj+ g0 g1 g2) (disj+ (all g0 g1) g2))
(define-syntax-rule (ife g0 g1 g2) (if-aux disj+  g0 g1 g2))
(define-syntax-rule (ifi g0 g1 g2) (if-aux disji+ g0 g1 g2))

(define-syntax cond-aux
  (syntax-rules (else)
    [(_ ifer) fail]
    [(_ ifer [else g ...]) (all g ...)]
    [(_ ifer [g ...]) (all g ...)]
    [(_ ifer [g0 g ...] c ...)
     (ifer g0 (all g ...) (cond-aux ifer c ...))]))
(define-syntax-rule (conde c ...) (cond-aux ife c ...))
(define-syntax-rule (condi c ...) (cond-aux ifi c ...))
