#lang racket/base

;; Run already-compiled book tests in a fresh process.  Startup is outside
;; the timed region; module loading, checks and their allocations are inside.
(require json raco/testing)
(define spec (read (open-input-string (vector-ref (current-command-line-arguments) 0))))
(collect-garbage)
(define allocated-before (current-memory-use 'cumulative))
(define-values (_ cpu real gc)
  (time-apply (λ () (dynamic-require spec #f)) '()))
(define allocated (- (current-memory-use 'cumulative) allocated-before))
(define checks (test-report))
(unless (zero? (car checks)) (error 'measure-aligned "test failures: ~a" checks))
(write-json (hasheq 'cpu_ms cpu 'real_ms real 'gc_ms gc
                    'allocated_bytes allocated 'checks (cdr checks)))
(newline)
