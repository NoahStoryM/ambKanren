#lang racket/base
;; Derive trs2-amb-empty.rkt from trs2-local-fused.rkt: the suspension is
;; (when (amb #t #f) (amb-empty)) (amb branch
;; research/amb-empty-20261109); the local search is
;; (in-amb/do (g s) #:empty-values (list PAUSED)).
;; Usage: racket gen-amb-empty.rkt trs2-local-fused.rkt trs2-amb-empty.rkt
(require racket/file racket/string)
(define-values (src dst) (values (vector-ref (current-command-line-arguments) 0)
                                 (vector-ref (current-command-line-arguments) 1)))
(define (rep s from to)
  (define r (string-replace s from to #:all? #f))
  (when (equal? r s) (error 'gen-amb-empty "pattern not found: ~a" from))
  r)
(define out
  (for/fold ([s (file->string src)])
            ([p (list
   (cons ";; FUSED VARIANT of trs2-local.rkt"
         ";; AMB-EMPTY VARIANT: as trs2-local-fused.rkt, but the suspension is\n;; (when (amb #t #f) (amb-empty)) and each local search declares\n;; #:empty-values (list PAUSED); its sequence type covers PAUSED.  A plain\n;; in-amb/do without #:empty-values only reschedules on a suspension.\n;;\n;; FUSED VARIANT of trs2-local.rkt")
   (cons "(: pause-tag (Prompt-Tagof (U Substitution Symbol) (→ Symbol)))\n(define pause-tag (make-continuation-prompt-tag 'kanren-pause))\n(define PAUSE : Symbol (string->uninterned-symbol \"PAUSE\"))\n"
         ";; The event a suspension outputs; ambKanren's own, not amb's.\n(struct paused ())\n(define PAUSED (paused))\n")
   (cons "(define (suspend)\n  (when (amb #t #f)\n    (abort-current-continuation pause-tag)))"
         "(define (suspend)\n  (when (amb #t #f)\n    (amb-empty)))")
   (cons "     (in-amb/do\n      (call-with-continuation-prompt\n       (λ () : (U Substitution Symbol) (g s))\n       pause-tag\n       (λ () PAUSE)))))"
         "     (in-amb/do (g s) #:empty-values (list PAUSED))))")
   (cons "(if (symbol? v) (λ () (loop)) (cons v (loop)))"
         "(if (paused? v) (λ () (loop)) (cons v (loop)))"))])
    (rep s (car p) (cdr p))))
(display-to-file out dst #:exists 'replace)
