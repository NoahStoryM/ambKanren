#lang typed/racket/base

(require "main.rkt")

(provide (all-defined-out))


(: caro (→ Term Term Goal))
(: cdro (→ Term Term Goal))
(: conso (→ Term Term Term Goal))
(define (caro p a) (fresh (d) (== (cons a d) p)))
(define (cdro p d) (fresh (a) (== (cons a d) p)))
(define (conso a d p) (== (cons a d) p))
