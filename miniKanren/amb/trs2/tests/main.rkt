#lang typed/racket/base

;; The examples of The Reasoned Schemer, second edition, from
;; miniKanren-typed.  typed/miniKanren/amb/trs2 schedules suspended computations
;; with amb's global frontier instead of append-inf, so some examples
;; produce their answers in a different order from the book.  This is
;; deliberate: the book's order breaks the laws of the search (see
;; "Order of Answers" in the documentation, and search.rkt).  Those examples are listed
;; in `known-order-differences` and checked less strictly:
;;
;;   same-answers  the same answers, in any order;
;;   same-count    a bounded run returns as many answers as the book,
;;                 but not necessarily the same ones (the book's answers
;;                 appear when more answers are requested).
;;
;; Every other example is checked with check-equal? as usual.

(require racket/include
         (except-in racket/match ==)
         (rename-in (except-in typed/rackunit fail)
                    [check-equal? rackunit:check-equal?])
         (only-in racket/list make-list)
         (for-syntax racket/base racket/path)
         "../main.rkt")

(begin-for-syntax
  (define known-order-differences
    #hash(["ch8.rktl:205" . same-answers]
          ["ch9.rktl:167" . same-answers]))
  (define (difference-kind stx)
    (define src (syntax-source stx))
    (and (path? src)
         (hash-ref known-order-differences
                   (format "~a:~a" (file-name-from-path src) (syntax-line stx))
                   #f))))

(: same-answers? (→ (Listof Any) (Listof Any) Boolean))
(define (same-answers? actual expected)
  (and (= (length actual) (length expected))
       (for/and : Boolean ([a (in-list actual)]) (and (member a expected) #t))
       (for/and : Boolean ([e (in-list expected)]) (and (member e actual) #t))))

(define-syntax (check-equal? stx)
  (syntax-case stx ()
    [(_ actual expected . more)
     (case (difference-kind stx)
       [(same-answers)
        (syntax/loc stx (check-true (same-answers? actual expected)))]
       [(same-count)
        (syntax/loc stx (rackunit:check-equal? (length actual) (length expected)))]
       [else
        (syntax/loc stx (rackunit:check-equal? actual expected . more))])]))

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
