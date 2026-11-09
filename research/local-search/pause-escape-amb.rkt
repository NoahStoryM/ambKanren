#lang typed/racket/base

;; The pause-escape counterexample (pause-escape.rkt) replayed against
;; trs2-amb-pause.rkt, whose suspension is amb's own `pause`.
;; Expected (1) and (1 2).  Run: racket pause-escape-amb.rkt

(require typed/amb "trs2-amb-pause.rkt")

(defrel (oneo x) (== x 1))

;; a direct-style goal running its own plain in-amb/do
(: raw-once (→ Goal Goal))
(define ((raw-once g) s)
  (define-values (more? get) (sequence-generate (in-amb/do (g s))))
  (if (more?) (get) (amb)))

(printf "raw in-amb/do around a pausing goal: ~s\n"
        (run* q (raw-once (oneo q)) (== q 1)))
(printf "same, wrapped by a local disj: ~s\n"
        (run* q (disj (raw-once (oneo q)) (== q 2))))
