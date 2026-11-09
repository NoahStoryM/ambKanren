#lang racket/base
;; Derive trs2-empty-values.rkt from trs2-local-fused.rkt: the suspension
;; is an empty (amb) under `current-amb-empty-values` (amb branch
;; research/amb-empty-values-20261109); the local search is a plain
;; in-amb/do and recognises the `paused` leaf itself.
;; Usage: racket gen-empty-values.rkt trs2-local-fused.rkt trs2-empty-values.rkt
(require racket/file racket/string)
(define-values (src dst) (values (vector-ref (current-command-line-arguments) 0)
                                 (vector-ref (current-command-line-arguments) 1)))
(define (rep s from to)
  (define r (string-replace s from to #:all? #f))
  (when (equal? r s) (error 'gen-empty-values "pattern not found: ~a" from))
  r)
(define out
  (for/fold ([s (file->string src)])
            ([p (list
   (cons ";; FUSED VARIANT of trs2-local.rkt"
         ";; EMPTY-VALUES VARIANT: as trs2-local-fused.rkt, but the suspension is\n;; an empty (amb) under current-amb-empty-values, whose leaf the local\n;; in-amb/do reports as an ordinary output.  amb knows nothing of pauses.\n;;\n;; FUSED VARIANT of trs2-local.rkt")
   (cons "(: pause-tag (Prompt-Tagof (U Substitution Symbol) (→ Symbol)))\n(define pause-tag (make-continuation-prompt-tag 'kanren-pause))\n(define PAUSE : Symbol (string->uninterned-symbol \"PAUSE\"))\n"
         ";; The event a suspension outputs; ambKanren's own, not amb's.\n(struct paused ())\n(define PAUSED (paused))\n")
   (cons "(define (suspend)\n  (when (amb #t #f)\n    (abort-current-continuation pause-tag)))"
         "(define (suspend)\n  (parameterize ([current-amb-empty-values (list PAUSED)])\n    (amb)))")
   (cons "     (in-amb/do\n      (call-with-continuation-prompt\n       (λ () : (U Substitution Symbol) (g s))\n       pause-tag\n       (λ () PAUSE)))))"
         "     ;; the type of (g s) does not mention the `paused` leaf; widen it\n     (in-amb/do (ann (g s) (U Substitution paused)))))")
   (cons "(if (symbol? v) (λ () (loop)) (cons v (loop)))"
         "(if (paused? v) (λ () (loop)) (cons v (loop)))"))])
    (rep s (car p) (cdr p))))
(display-to-file out dst #:exists 'replace)
