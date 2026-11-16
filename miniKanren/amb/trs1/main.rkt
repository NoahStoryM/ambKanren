#lang typed/racket/base

;; The user-level language of ambKanren.
;;
;; The interface follows The Reasoned Schemer, first edition (Friedman,
;; Byrd, and Kiselyov, MIT Press, 2005).  The cond-aux and all-aux macros
;; keep the structure of the book's appendix A; run and the
;; if-forms are rewritten on top of amb.  The internal goal combinators
;; and delay forms are adapted from microKanren.  See NOTICE.

(require "base.rkt"
         (except-in racket/match ==)
         typed/amb typed/amb/merge
         "../../../private/search.rkt")

(provide (all-from-out "base.rkt")
         run run* λG fresh
         all alli all-aux
         ife ifi ifa ifu
         conde condi conda condu cond-aux)


(: disj (→ Goal * Goal))
(define (disj . gs)
  (λ (s)
    ((parameterize ([current-amb-merger amb-append]) (sequence->amb gs)) s)))

(: conj (→ Goal * Goal))
(define (conj . gs)
  (cond
    [(null? gs) succeed]
    [(null? (cdr gs)) (car gs)]
    [else
     (define first (car gs))
     (define rest (apply conj (cdr gs)))
     (λ (s) (rest (parameterize ([current-amb-merger amb-append])
                   (amb-local* (λ () (first s))))))]))

(: disji (→ Goal * Goal))
(define (disji . gs)
  (λ (s)
    ((parameterize ([current-amb-merger amb-alternate]) (sequence->amb gs)) s)))

(: conji (→ Goal * Goal))
(define (conji . gs)
  (cond
    [(null? gs) succeed]
    [(null? (cdr gs)) (car gs)]
    [else
     (define first (car gs))
     (define rest (apply conji (cdr gs)))
     (λ (s) (rest (parameterize ([current-amb-merger amb-alternate])
                   (amb-local* (λ () (first s))))))]))


(define-syntax-rule (run n (x) g* ...)
  (let ([x (var 'x)])
    (map (λ ([s : Substitution]) (reify (walk* x s)))
         (take-answers n (λ () ((all g* ...) empty-s))))))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))

(define-syntax-rule (λG (x) body ...) (ann (λ (x) body ...) Goal))
(define-syntax-rule (Zzz g) (λG (s) (g s)))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x ...) g ...)
     (Zzz (let ([x (var 'x)] ...) (all g ...)))]))

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
(define-syntax-rule (ifa g0 g1 g2)
  (λG (s)
    (define-values (v next) (first-answer (λ () (g0 s))))
    (if v
        (g1 (parameterize ([current-amb-merger amb-append])
              (make-amb (λ () (values v next)))))
        (g2 s))))
(define-syntax-rule (ifu g0 g1 g2)
  (λG (s)
    (define-values (v next) (first-answer (λ () (g0 s))))
    (if v (g1 (car v)) (g2 s))))

(define-syntax cond-aux
  (syntax-rules (else)
    [(_ ifer) fail]
    [(_ ifer [else g ...]) (all g ...)]
    [(_ ifer [g ...]) (all g ...)]
    [(_ ifer [g0 g ...] c ...)
     (ifer g0 (all g ...) (cond-aux ifer c ...))]))
(define-syntax-rule (conde c ...) (cond-aux ife c ...))
(define-syntax-rule (condi c ...) (cond-aux ifi c ...))
(define-syntax-rule (conda c ...) (cond-aux ifa c ...))
(define-syntax-rule (condu c ...) (cond-aux ifu c ...))
