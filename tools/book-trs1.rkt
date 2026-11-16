#lang typed/racket/base
(require racket/include (except-in racket/match ==)
         racket/list (for-syntax racket/base racket/path)
         typed/amb typed/amb/merge
         typed/miniKanren/amb/trs1)
(require/typed "book-record.rkt"
  [record! (→ String (→ Any) Any Void)]
  [finish! (→ String (Option String) Void)])
(define strategy (or (getenv "AMB_MERGER") "interleave"))
(current-amb-merger
 (case (string->symbol strategy)
   [(append) amb-append] [(interleave) amb-interleave]
   [(alternate) amb-alternate] [(level) amb-level]
   [else (error "unknown merger")]))
(define-syntax (check-equal? stx)
  (syntax-case stx ()
    [(_ actual expected . _)
     (with-syntax ([location
                    (format "~a:~a" (file-name-from-path (syntax-source stx)) (syntax-line stx))])
       #'(record! location (λ () actual) expected))]))
(include (lib "typed/miniKanren/trs1/tests/ch1.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch2.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch3.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch4.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch5.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch6.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch7.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch8.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch9.rktl"))
(include (lib "typed/miniKanren/trs1/tests/ch10.rktl"))
(finish! (string-append "trs1-" strategy) (getenv "BOOK_RECORD"))
