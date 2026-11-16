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
