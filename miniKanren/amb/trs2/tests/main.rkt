#lang typed/racket/base

;; The examples of The Reasoned Schemer, second edition, from
;; miniKanren-typed.  conj2 binds locally, as the book does, so with
;; amb-interleave every example is checked with check-equal? (see
;; "Order of Answers" in the documentation).

(require racket/include
         (except-in racket/match ==)
         (except-in typed/rackunit fail)
         (only-in racket/list make-list)
         "../main.rkt"
         typed/amb typed/amb/merge)

(current-amb-merger amb-interleave)

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
