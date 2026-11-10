#lang typed/racket/base

;; The pause-escape counterexample (pause-escape.rkt) replayed against
;; trs2-amb-handler.rkt.  Expected (1) and (1 2).
;; The last line is the cost of the design: a hand-written goal that
;; fails with (amb) instead of (amb-fail) is answered by the handler,
;; i.e. it pauses and ends; its answers are unchanged.
;; Run: racket pause-escape-amb-handler.rkt

(require typed/amb "trs2-amb-handler.rkt")

(defrel (oneo x) (== x 1))

;; a direct-style goal running its own plain in-amb/do
(: raw-once (→ Goal Goal))
(define ((raw-once g) s)
  (define-values (more? get) (sequence-generate (in-amb/do (g s))))
  (if (more?) (get) (amb-fail)))

(printf "raw in-amb/do around a pausing goal: ~s\n"
        (run* q (raw-once (oneo q)) (== q 1)))
(printf "same, wrapped by a local disj: ~s\n"
        (run* q (disj (raw-once (oneo q)) (== q 2))))

(: amb-fails Goal)
(define (amb-fails s) (amb))
(printf "a goal that fails with (amb): ~s\n"
        (run* q (disj amb-fails (== q 3))))
