#lang typed/racket/base

;; microKanren with streams replaced by amb.
;; The definitions are in amb/; see NOTICE.

(require "amb/main.rkt")
(provide (all-from-out "amb/main.rkt"))
