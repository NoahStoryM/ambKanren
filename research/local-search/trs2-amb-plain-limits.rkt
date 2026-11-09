#lang typed/racket/base

;; What trs2-amb-plain.rkt (amb master, no extension) does when a
;; direct-style goal calls a combinator goal in its body.
;; Run: racket trs2-amb-plain-limits.rkt

(require (except-in typed/rackunit fail) "trs2-amb-plain.rkt")

(defrel (nevero) (nevero))

(: timed (→ (→ Any) Any))
(define (timed thk)
  (define ch : (Channelof Any) (make-channel))
  (define th (thread (λ () (channel-put ch (thk)))))
  (define r (sync/timeout 3 ch))
  (unless r (kill-thread th))
  (or r 'TIMEOUT-3s))

;; 1. Works: combinator goal in the middle of a direct-style goal, with
;;    more direct-style code after it.  Answers keep their order.
(check-equal?
 (run* (q r)
   (ann (λ (s) ((== r 'x) ((disj (== q 1) (== q 2)) s))) Goal))
 '((1 x) (2 x)))

;; 2. Works: the subgoal pauses, but its stream still matures
;;    (append-inf swaps at the pause), so forcing it terminates.
(check-equal?
 (run 1 q (ann (λ (s) ((disj (nevero) (== q 'ok)) s)) Goal))
 '(ok))

;; 3. The limit: a direct-style goal whose combinator subgoal pauses
;;    forever without an answer.  `reflect` forces those suspensions, so
;;    the direct-style goal never returns and its sibling never runs.
;;    With the combinator goal itself, the sibling answers.
(check-equal? (timed (λ () (run 1 q (disj (nevero) (== q 'R))))) '(R))
(check-equal? (timed (λ () (run 1 q (disj (ann (λ (s) ((nevero) s)) Goal) (== q 'R)))))
              'TIMEOUT-3s)

(displayln "trs2-amb-plain limits: 4 checks done")
