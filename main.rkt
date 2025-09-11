#lang typed/racket/base

(require "milli.rkt"
         "base.rkt"
         (except-in racket/match ==)
         typed/amb)

(provide (all-from-out "base.rkt")
         run run* (rename-out [run* run∞])
         disji+ conji+
         alli
         condi ifi
         (rename-out
          [disji+ ∨i]
          [conji+ ∧i]))


(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real (or n^ +inf.0)]
        [x (var 0)]
        [g (fresh (x) g* ...)]
        [s/c (state empty-s 0)])
    (define s/c*
      (parameterize ([current-amb-rotator void]
                     [current-amb-shuffler void]
                     [current-amb-maker make-tasks]
                     [current-amb-length tasks-length]
                     [current-amb-pusher tasks-add!]
                     [current-amb-popper tasks-del!])
        (in-amb/do (g s/c))))
    (for/list : (Listof Term)
              ([s/c s/c*]
               [_ (in-range n)])
      (match-define (state s c) s/c)
      (reify (walk* x s)))))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))

(define-syntax disji+
  (syntax-rules ()
    [(_) fail]
    [(_ g ...) (Zzz (disji g ...))]))

(define-syntax conji+
  (syntax-rules ()
    [(_) succeed]
    [(_ g ...) (Zzz (conji g ...))]))

(define-syntax-rule (alli g ...) (all-aux conji+ g ...))

(define-syntax-rule (condi c ...) (cond-aux disji+ c ...))
(define-syntax-rule (ifi g0 g1 g2) (condi [g0 g1] [else g2]))
