#lang typed/racket/base

(require "micro.rkt"
         "base.rkt")

(provide (all-from-out "base.rkt")
         condi ifi)


(define-syntax-rule (condi c ...) (cond-aux disji c ...))
(define-syntax-rule (ifi g0 g1 g2) (condi [g0 g1] [else g2]))
