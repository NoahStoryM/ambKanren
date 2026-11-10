#lang racket/base

;; Run programs.rktl under every scheme and compare with the TRS2
;; reference (reference-trs2.out, produced by reference-trs2.rkt).
;; Run: racket compare.rkt
;;
;; A run that exceeds `fuel` suspensions is reported as STARVED (it
;; would otherwise not terminate); a run over the time limit as TIMEOUT.

(require racket/include racket/list racket/runtime-path
         "kernel.rkt" "schemes.rkt")

(include "programs.rktl")

(define-runtime-path ref-file "reference-trs2.out")
(define reference
  (with-input-from-file ref-file
    (λ () (for/hasheq ([e (in-port read)]) (values (car e) (cdr e))))))

(define (run-limited thk secs)
  (define ch (make-channel))
  (define th
    (thread (λ ()
              (channel-put
               ch
               (with-handlers ([exn:starved?
                                (λ (e) (list 'STARVED (exn:starved-pauses e)))])
                 (list 'ok (thk)))))))
  (define r (sync/timeout secs ch))
  (unless r (kill-thread th))
  (or r (list 'TIMEOUT secs)))

(define (pad s) (let ([s (format "~a" s)]) (string-append s (make-string (max 0 (- 11 (string-length s))) #\space))))
(define schemes (list global answers pause/disj pause/full))

(define (classify got ref)
  (cond [(not (eq? (car got) 'ok)) (format "~a" got)]
        [(equal? (cadr got) ref) "= same order"]
        [(and (= (length (cadr got)) (length ref))
              (equal? (sort (map (λ (x) (format "~s" x)) (cadr got)) string<?)
                      (sort (map (λ (x) (format "~s" x)) ref) string<?)))
         (format "~~ same answers, other order: ~s" (cadr got))]
        [else (format "≠ ~s" (cadr got))]))

(define summary (make-hasheq))
(for ([p (in-list programs)])
  (define name (car p))
  (define ref (hash-ref reference name))
  (printf "~a\n  reference (TRS2 streams): ~s\n" name ref)
  (for ([sc (in-list schemes)])
    (use-scheme! sc)
    (reset-counters!)
    (define got (parameterize ([fuel 20000]) (run-limited (cdr p) 20)))
    (define c (classify got ref))
    (hash-update! summary (scheme-name sc)
                  (λ (l) (cons (string-ref c 0) l)) '())
    (printf "  ~a: ~a   [pauses ~a, relays ~a, local searches ~a]\n"
            (pad (scheme-name sc)) c
            (counter 'pause) (counter 'relay) (counter 'spawn))))

(printf "\nsummary (= same order, ~~ same answers/other order, ≠ different, ( starved/timeout):\n")
(for ([sc (in-list schemes)])
  (define l (hash-ref summary (scheme-name sc)))
  (printf "  ~a ~a\n" (pad (scheme-name sc))
          (for/list ([k '(#\= #\~ #\≠ #\()])
            (cons k (count (λ (c) (char=? c k)) l)))))
