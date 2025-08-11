#lang typed/racket/base

(require "main.rkt")

(provide (all-defined-out))


(: pairo (→ Term Goal))
(: caro (→ Term Term Goal))
(: cdro (→ Term Term Goal))
(: conso (→ Term Term Term Goal))
(: nullo (→ Term Goal))
(define (pairo p) (fresh (a d) (== (cons a d) p)))
(define (caro p a) (fresh (d) (== (cons a d) p)))
(define (cdro p d) (fresh (a) (== (cons a d) p)))
(define (conso a d p) (== (cons a d) p))
(define (nullo x) (== '() x))

(: eqo (→ Term Term Goal))
(define (eqo x y) (== x y))
