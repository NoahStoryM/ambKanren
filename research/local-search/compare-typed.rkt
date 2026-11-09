#lang typed/racket/base

;; The 13 programs of programs.rktl under a typed TRS2 implementation,
;; compared with reference-trs2.out.  Run: racket compare-typed.rkt
;; (edit the require to choose the implementation; default trs2-exn.rkt)

(require racket/include racket/runtime-path "trs2-exn.rkt")

(include "programs.rktl")

(define-runtime-path ref-file "reference-trs2.out")
(define reference
  (with-input-from-file ref-file
    (λ () (for/list : (Listof Any) ([e (in-port read)]) e))))

(: timed (→ (→ Any) Any))
(define (timed thk)
  (define ch : (Channelof Any) (make-channel))
  (define th (thread (λ () (channel-put ch (thk)))))
  (define r (sync/timeout 20 ch))
  (unless r (kill-thread th))
  (or r 'TIMEOUT-20s))

(for ([p (in-list programs)] [ref (in-list reference)])
  (define got (timed (cdr p)))
  (printf "~a ~a\n" (if (equal? (cons (car p) got) ref) "=" "≠") (car p))
  (unless (equal? (cons (car p) got) ref)
    (printf "    got ~s\n    ref ~s\n" got ref)))
