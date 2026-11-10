#lang typed/racket/base

;; Side check: in TRS1 (first edition) a goal application runs until
;; its first answer or failure; there is no suspension boundary that
;; condi/alli could use.  Expect the first run to time out.
;; Run: racket trs1-no-pause.rkt

(require typed/miniKanren/trs1)

(: anyo (→ Goal Goal))
(define (anyo g) (conde [g succeed] [else (anyo g)]))
(define nevero (anyo fail))

(: timed (→ (→ Any) Any))
(define (timed thk)
  (define ch : (Channelof Any) (make-channel))
  (define th (thread (λ () (channel-put ch (thk)))))
  (define r (sync/timeout 5 ch))
  (unless r (kill-thread th))
  (or r 'TIMEOUT-5s))

(printf "(run 1 q (condi [nevero] [else (== q 'R)])) => ~s\n"
        (timed (λ () (run 1 (q) (condi [nevero] [else (== q 'R)])))))
(printf "(run 1 q (condi [(== q 'L) nevero] [else (== q 'R)])) => ~s\n"
        (timed (λ () (run 1 (q) (condi [(== q 'L) nevero] [else (== q 'R)])))))
