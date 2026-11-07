#lang typed/racket/base

(require racket/include typed/rackunit "../main.rkt")

(include "programs.rktl")
(include "core.rktl")
(include (lib "typed/microKanren/tests/wrappers.rktl"))

;; A zero bound must not even construct the local goal search.
(check-equal? (take 0 (λ () (error 'take "unexpected evaluation"))) '())
