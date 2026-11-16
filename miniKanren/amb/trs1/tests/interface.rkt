#lang typed/racket/base

(require (except-in typed/rackunit fail)
         typed/miniKanren/amb/trs1)

;; Chapter 9's project is an exercise, not part of the library interface.
(define-syntax project
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x ...) g ...)
     (λG (s) (let ([x (walk* x s)] ...) ((all g ...) s)))]))

(check-equal?
 (run* (q)
   (== #f q)
   (project (q)
     (== (not (not q)) q)))
 '(#f))
