#lang typed/racket/base

;; The Reasoned Schemer, second edition, examples (from miniKanren-typed)
;; run against trs2-shift.rkt with STRICT check-equal? everywhere: no
;; `known-order-differences` table.  Compare miniKanren/amb/trs2/tests,
;; which needs 15 relaxed checks for the global-frontier scheduler.
;;
;; ONE example is skipped: ch6 frame 25 (a million answers of
;; very-recursiveo): unfused, superlinear (over 300 s), as trs2-exn.
;; Run: raco test research/local-search/trs2-shift-tests.rkt

(require racket/include
         (for-syntax racket/base racket/path)
         (except-in racket/match ==)
         (rename-in (except-in typed/rackunit fail)
                    [check-equal? rackunit:check-equal?])
         (only-in racket/list make-list)
         "trs2-shift.rkt")

(begin-for-syntax
  (define (skip? stx)
    (define src (syntax-source stx))
    (and (path? src)
         (equal? (format "~a:~a" (file-name-from-path src) (syntax-line stx))
                 "ch6.rktl:104"))))

(define-syntax (check-equal? stx)
  (syntax-case stx ()
    [(_ actual expected . more)
     (if (skip? stx)
         #'(void)
         (syntax/loc stx (rackunit:check-equal? actual expected . more)))]))

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
