#lang typed/racket/base

(require racket/include
         (except-in racket/match ==)
         (except-in typed/rackunit fail)
         (only-in racket/list make-list)
         "../main.rkt")

(include (lib "typed/miniKanren/trs2/tests/ch1.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch2.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch3.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch4.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch5.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch6.rktl"))
(include (lib "typed/miniKanren/trs2/tests/arith.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch7.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch8.rktl"))
(include (lib "typed/miniKanren/trs2/tests/ch9.rktl"))

(check-equal?
 (run-goal 0 (λ ([s : Substitution]) (error 'run-goal "unexpected evaluation")))
 '())

;; IFTE constructs its fallback only after the test search is exhausted.
(check-equal?
 (run* q (ifte succeed succeed
               (λ ([s : Substitution]) (error 'ifte "unexpected fallback"))))
 '(_0))

;; Failure in THEN does not reopen the committed test or select ELSE.
(check-equal? (run* q (ifte succeed fail succeed)) '())
