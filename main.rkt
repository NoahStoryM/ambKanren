#lang typed/racket/base

(require "milli.rkt"
         "base.rkt"
         (except-in racket/match ==)
         typed/amb)

(provide (all-from-out "base.rkt")
         run run* (rename-out [run* run∞])
         disji+
         condi ifi)


(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real (or n^ +inf.0)]
        [x (var 0)]
        [g (fresh (x) g* ...)]
        [s/c (state empty-s 0)])
    (for/list : (Listof Term)
              ([s/c (in-amb (g s/c))]
               [_ (in-range n)])
      (match-define (state s c) s/c)
      (reify (walk* x s)))))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))

(define-syntax disji+
  (syntax-rules ()
    [(_) fail]
    [(_ g ...) (Zzz (disji g ...))]))

(define-syntax-rule (condi c ...) (cond-aux disji+ c ...))
(define-syntax-rule (ifi g0 g1 g2) (condi [g0 g1] [else g2]))
