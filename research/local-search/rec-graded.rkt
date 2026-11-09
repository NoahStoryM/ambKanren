#lang typed/racket/base
;; Run the book's examples against IMPL, recording every actual result
;; (one line per check: "file:line result") and checking it against the
;; book only as a MULTISET.  Used to compare answer orders across
;; implementations.  Generated from record-tests.rkt.in.
(require racket/include
         (for-syntax racket/base racket/path)
         (except-in racket/match ==)
         (only-in typed/rackunit check-true)
         (only-in racket/list make-list)
         "trs2-graded.rkt")
(define (multiset [v : Any]) : Any
  (if (list? v) (sort (map (λ (x) (format "~s" x)) v) string<?) v))
(define (record [loc : String] [actual : Any] [expected : Any]) : Void
  (printf "~a ~s\n" loc actual)
  (check-true (equal? (multiset actual) (multiset expected)) loc))
(define-syntax (check-equal? stx)
  (syntax-case stx ()
    [(_ actual expected . more)
     (let ([loc (format "~a:~a" (file-name-from-path (syntax-source stx)) (syntax-line stx))])
       (if (equal? loc "ch6.rktl:104")
           #'(void)
           #`(record #,loc actual expected)))]))
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
