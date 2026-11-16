#lang typed/racket/base

;; Conjunction binds locally, as in the stream reference, so every
;; result is checked for exact equality.

(require racket/include
         typed/rackunit
         "../main.rkt"
         typed/amb typed/amb/merge)

(current-amb-merger amb-interleave)

(include "programs.rktl")
(include "core.rktl")
(include (lib "typed/microKanren/tests/wrappers.rktl"))

;; unify fails with amb-fail instead of returning #f
(check-equal? (take-all (λ () (unify (var 0) 5 '()))) (list (list (cons (var 0) 5))))
(check-equal? (take-all (λ () (unify '(1 2) '(1 3) '()))) '())
(check-equal? (take-all (λ () (unify (cons (var 0) 2) (cons 1 (var 1)) '())))
              (list (list (cons (var 1) 2) (cons (var 0) 1))))
