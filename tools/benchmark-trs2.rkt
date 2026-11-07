#lang racket/base

;; Compile before timing. Run the same chapter examples in a fresh process
;; for each round, preserving the checkout's own assertions and exceptions.
;; Usage: racket tools/benchmark-trs2.rkt --rounds 3 /path/to/checkout
;; Accepts either ambKanren or miniKanren-typed checkouts.
(module+ main
  (require racket/cmdline racket/file racket/path racket/string racket/system)

  (define rounds 3)
  (define checkout
    (command-line
     #:once-each
     [("--rounds") n "Number of fresh-process runs (default 3)"
      (define parsed (string->number n))
      (unless (exact-positive-integer? parsed)
        (raise-user-error 'benchmark-trs2 "invalid round count: ~a" n))
      (set! rounds parsed)]
     #:args (checkout) (path->complete-path checkout)))
  (define amb-tests (build-path checkout "miniKanren" "amb" "trs2" "tests"))
  (define tests
    (if (directory-exists? amb-tests)
        amb-tests
        (build-path checkout "miniKanren" "trs2" "tests")))
  (define source (file->string (build-path tests "main.rkt")))
  (define runner (make-temporary-file "hierarchical-bench-~a.rkt" #f tests))
  (define raco
    (build-path (path-only (path->complete-path (find-system-path 'exec-file)))
                "raco"))
  (define (clean)
    (delete-file runner)
    (define compiled (build-path tests "compiled"))
    (define stem (path->string (path-replace-extension (file-name-from-path runner) #"")))
    (when (directory-exists? compiled)
      (for ([file (in-list (directory-list compiled))]
            #:when (string-prefix? (path->string file) stem))
        (delete-file (build-path compiled file)))))
  (dynamic-wind
   void
   (lambda ()
     (call-with-output-file runner #:exists 'truncate
       (lambda (out)
         (define index 0)
         (for ([line (in-list (string-split source "\n" #:trim? #f))])
           (cond
             ;; ambKanren's shared reference suite ends at chapter 9.
             [(regexp-match? #rx"^\\(include .*ch10[.]rktl" line) (void)]
             [(regexp-match #rx"^\\(include .*\"([^\"]+)[.]rktl\"" line)
              =>
              (lambda (match)
                (set! index (add1 index))
                (define chapter (file-name-from-path (cadr match)))
                (fprintf out "(collect-garbage)\n")
                (fprintf out "(define cpu~a (current-process-milliseconds))\n" index)
                (fprintf out "(define real~a (current-inexact-milliseconds))\n" index)
                (fprintf out "(define gc~a (current-gc-milliseconds))\n" index)
                (displayln line out)
                (fprintf out
                         "(printf ~s (- (current-process-milliseconds) cpu~a) (- (current-inexact-milliseconds) real~a) (- (current-gc-milliseconds) gc~a))\n"
                         (format "BENCH ~a cpu=~~a real=~~a gc=~~a ms\n" chapter)
                         index index index)
                (fprintf out "(flush-output)\n"))]
             [else (displayln line out)]))))
     (printf "Checkout: ~a; Racket ~a; ~a measured runs; no warmup\n"
             checkout (version) rounds)
     (flush-output)
     (unless (system* raco "make" runner)
       (raise-user-error 'benchmark-trs2 "runner compilation failed"))
     (for ([round (in-range 1 (add1 rounds))])
       (printf "Round ~a\n" round)
       (flush-output)
       (unless (system* raco "test" runner)
         (raise-user-error 'benchmark-trs2 "tests failed in round ~a" round))))
   clean))
