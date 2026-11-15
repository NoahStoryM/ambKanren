#lang typed/racket/base

;; The stream reference and direct-style conjunction can return a
;; different bounded prefix.  Keep the explicitly identified wrapper
;; comparisons; all other results are checked for exact equality.

(require racket/include
         (rename-in typed/rackunit
                    [check-equal? rackunit:check-equal?])
         (for-syntax racket/base racket/path)
         "../main.rkt"
         typed/amb typed/amb/merge)

(current-amb-merger amb-interleave)

(begin-for-syntax
  (define known-order-differences
    #hash(["wrappers.rktl:37" . same-answers]
          ["wrappers.rktl:66" . same-count]))
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

(include "programs.rktl")
(include "core.rktl")
(include (lib "typed/microKanren/tests/wrappers.rktl"))
