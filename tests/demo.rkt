#lang typed/racket/base

(require racket/include
         (except-in racket/match ==)
         (except-in typed/rackunit fail)
         "../demo.rkt")

(include (lib "typed/miniKanren/tests/trs1/ch1.rktl"))
(include (lib "typed/miniKanren/tests/trs1/ch2.rktl"))
(include (lib "typed/miniKanren/tests/trs1/ch3.rktl"))
(include (lib "typed/miniKanren/tests/trs1/ch4.rktl"))
(include (lib "typed/miniKanren/tests/trs1/ch5.rktl"))
#;(include (lib "typed/miniKanren/tests/trs1/ch6.rktl"))
#;(include (lib "typed/miniKanren/tests/trs1/ch7.rktl"))
#;(include (lib "typed/miniKanren/tests/trs1/ch8.rktl"))
(include (lib "typed/miniKanren/tests/trs1/ch9.rktl"))
#;(include (lib "typed/miniKanren/tests/trs1/ch10.rktl"))
