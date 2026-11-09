#lang racket/base
;; Derive trs2-amb-output.rkt from trs2-local-fused.rkt: the suspension is
;; (when (amb #t #f) (amb-output PAUSED)) (amb branch
;; research/amb-output-20261109); the local search is a plain
;; in-amb/do and recognises the `paused` leaf itself.
;; Usage: racket gen-amb-output.rkt trs2-local-fused.rkt trs2-amb-output.rkt
(require racket/file racket/string)
(define-values (src dst) (values (vector-ref (current-command-line-arguments) 0)
                                 (vector-ref (current-command-line-arguments) 1)))
(define (rep s from to)
  (define r (string-replace s from to #:all? #f))
  (when (equal? r s) (error 'gen-amb-output "pattern not found: ~a" from))
  r)
(define out
  (for/fold ([s (file->string src)])
            ([p (list
   (cons ";; FUSED VARIANT of trs2-local.rkt"
         ";; AMB-OUTPUT VARIANT: as trs2-local-fused.rkt, but the suspension is\n;; (when (amb #t #f) (amb-output PAUSED)): an output from the middle of\n;; the computation, whose rest stays as the second alternative.  The local\n;; in-amb/do reports it as an ordinary output; amb knows nothing of pauses.\n;;\n;; FUSED VARIANT of trs2-local.rkt")
   (cons "(: pause-tag (Prompt-Tagof (U Substitution Symbol) (→ Symbol)))\n(define pause-tag (make-continuation-prompt-tag 'kanren-pause))\n(define PAUSE : Symbol (string->uninterned-symbol \"PAUSE\"))\n"
         ";; The event a suspension outputs; ambKanren's own, not amb's.\n(struct paused ())\n(define PAUSED (paused))\n")
   (cons "(define (suspend)\n  (when (amb #t #f)\n    (abort-current-continuation pause-tag)))"
         "(define (suspend)\n  (when (amb #t #f)\n    (amb-output PAUSED)))")
   (cons "     (in-amb/do\n      (call-with-continuation-prompt\n       (λ () : (U Substitution Symbol) (g s))\n       pause-tag\n       (λ () PAUSE)))))"
         "     ;; the type of (g s) does not mention the `paused` leaf; widen it\n     (in-amb/do (ann (g s) (U Substitution paused)))))")
   (cons "(if (symbol? v) (λ () (loop)) (cons v (loop)))"
         "(if (paused? v) (λ () (loop)) (cons v (loop)))"))])
    (rep s (car p) (cdr p))))
(display-to-file out dst #:exists 'replace)
