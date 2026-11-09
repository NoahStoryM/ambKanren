#lang racket/base
;; Derive trs2-amb-pause.rkt from trs2-local-fused.rkt: replace the
;; private pause prompt tag with amb's own `pause` / `in-amb/pause`
;; (amb branch research/observable-pause-20261109).
;; Usage: racket gen-amb-pause.rkt trs2-local-fused.rkt trs2-amb-pause.rkt
(require racket/file racket/string)
(define-values (src dst) (values (vector-ref (current-command-line-arguments) 0)
                                 (vector-ref (current-command-line-arguments) 1)))
(define (rep s from to)
  (define r (string-replace s from to #:all? #f))
  (when (equal? r s) (error 'gen-amb-pause "pattern not found: ~a" from))
  r)
(define out
  (for/fold ([s (file->string src)])
            ([p (list
   (cons ";; FUSED VARIANT of trs2-local.rkt"
         ";; AMB-PAUSE VARIANT: as trs2-local-fused.rkt, but the suspension is amb's\n;; own `pause` observed with `in-amb/pause` (no extra prompt tag), so a\n;; pause can never escape the search that contains it.\n;;\n;; FUSED VARIANT of trs2-local.rkt")
   (cons "(: pause-tag (Prompt-Tagof (U Substitution Symbol) (→ Symbol)))\n(define pause-tag (make-continuation-prompt-tag 'kanren-pause))\n(define PAUSE : Symbol (string->uninterned-symbol \"PAUSE\"))\n"
         "")
   (cons "(define (suspend)\n  (when (amb #t #f)\n    (abort-current-continuation pause-tag)))"
         "(define (suspend) (pause))")
   (cons "     (in-amb/do\n      (call-with-continuation-prompt\n       (λ () : (U Substitution Symbol) (g s))\n       pause-tag\n       (λ () PAUSE)))))"
         "     (in-amb/pause (g s))))"))])
    (rep s (car p) (cdr p))))
(display-to-file out dst #:exists 'replace)
