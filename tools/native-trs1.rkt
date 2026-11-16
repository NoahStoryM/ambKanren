#lang racket/base

;; Keep benchmark-only imports in a test submodule (build dependencies).
(module test typed/racket/base

  ;; Exactly the chapters exercised by ambKanren; use the native implementation
  ;; and the book's original assertions, without answer-order exceptions.
  (require racket/include
           (except-in racket/match ==)
           (except-in typed/rackunit fail)
           typed/miniKanren/trs1)

  (include (lib "typed/miniKanren/trs1/tests/ch1.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch2.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch3.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch4.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch5.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch6.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch7.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch8.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch9.rktl"))
  (include (lib "typed/miniKanren/trs1/tests/ch10.rktl"))
)
