#lang typed/racket/base

;; The Reasoned Schemer, second edition, examples (from miniKanren-typed)
;; run against trs2-empty-values.rkt with STRICT check-equal? everywhere: no
;; `known-order-differences` table.  Compare miniKanren/amb/trs2/tests,
;; which needs 15 relaxed checks for the global-frontier scheduler.
;; Run: raco test research/local-search/trs2-local-tests.rkt

(require racket/include
         (except-in racket/match ==)
         (except-in typed/rackunit fail)
         (only-in racket/list make-list)
         "trs2-empty-values.rkt")

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
