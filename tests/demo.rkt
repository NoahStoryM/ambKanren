#lang typed/racket/base

(require racket/include
         (except-in racket/match ==)
         (except-in typed/rackunit fail)
         "../demo.rkt")

(include (lib "typed/miniKanren/tests/trs2/ch1.rktl"))
(include (lib "typed/miniKanren/tests/trs2/ch2.rktl"))
(include (lib "typed/miniKanren/tests/trs2/ch3.rktl"))
(include (lib "typed/miniKanren/tests/trs2/ch4.rktl"))
(include (lib "typed/miniKanren/tests/trs2/ch5.rktl"))
#;(include (lib "typed/miniKanren/tests/trs2/ch6.rktl"))
#;(include (lib "typed/miniKanren/tests/trs2/ch7.rktl"))
#;(include (lib "typed/miniKanren/tests/trs2/ch8.rktl"))
(include (lib "typed/miniKanren/tests/trs2/ch9.rktl"))
#;(include (lib "typed/miniKanren/tests/trs2/ch10.rktl"))

