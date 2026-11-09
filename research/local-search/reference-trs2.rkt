#lang typed/racket/base

;; Reference answers: the same programs under miniKanren-typed TRS2
;; (stream-based append-inf / append-map-inf, as in trs2-impl.scm).
;; Prints one `(name . answers)` per line, `read`able by compare.rkt.

(require racket/include typed/miniKanren/trs2)

(include "programs.rktl")

(for ([p (in-list programs)])
  (writeln (cons (car p) ((cdr p)))))
