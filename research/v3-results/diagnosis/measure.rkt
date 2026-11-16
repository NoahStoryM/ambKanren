#lang racket/base
(define spec (read (open-input-string (vector-ref (current-command-line-arguments) 0))))
(define m0 (current-memory-use 'cumulative))
(define-values (_ cpu real gc) (time-apply (λ () (dynamic-require spec #f)) '()))
(printf "cpu ~a real ~a gc ~a alloc-MB ~a\n" cpu real gc (quotient (- (current-memory-use 'cumulative) m0) 1000000))
