#lang racket/base

;; Execution traces for the issue report.  Run: racket traces.rkt
;;
;; Events:
;;   (spawn id label)  a local search over (g s) is created (Spawn)
;;   (yield id)        that local search delivers an answer   (Yield)
;;   (pause id)        it reaches a suspension boundary       (Pause)
;;   (done id)         it is exhausted                        (Done)
;;   (suspend)         a defrel suspension point is executed
;;   (goal x)          the test goal x actually starts running
;;   (answer v)        the top-level run receives answer v

(require racket/list "kernel.rkt" "schemes.rkt")

(define (show title sc thk #:limit [limit 40])
  (use-scheme! sc)
  (reset-counters!)
  (define-values (r log)
    (with-trace
     (λ ()
       (with-handlers ([exn:starved? (λ (e) (list 'STARVED (exn:starved-pauses e)))])
         (parameterize ([fuel 2000]) (thk))))))
  (printf "== ~a  [scheme ~a]\nresult: ~s\n" title (scheme-name sc) r)
  (define n (length log))
  (for ([ev (in-list (take log (min n limit)))]) (printf "  ~s\n" ev))
  (when (> n limit) (printf "  ... ~a more events\n" (- n limit)))
  (newline))

;; a goal that records when it starts, then behaves like g
(define ((noisy x g) s) (trace! 'goal x) (g s))

(define q (var 'q))
(define (ans v) (λ (s) (trace! 'answer v) s))

;; T1: ordinary local interleaving, finite, nested disj under conj.
;; Shows lazy initialisation: the right branch's local search is only
;; spawned when its turn comes (answers), versus eagerly up to its first
;; suspension (pause/*: TRS2 append-inf evaluates (g2 s) eagerly).
(defrel (twoo x a b) (conde [(== x a)] [(== x b)]))
(define (T1)
  (run* q (disj (noisy 'L (twoo q 1 2))
                (noisy 'R (twoo q 10 20)))))
(show "T1 finite local interleaving" answers T1)
(show "T1 finite local interleaving" pause/disj T1)

;; T2: the counterexample. Left suspends forever, right succeeds now.
(defrel (nevero) (nevero))
(define (T2) (run 1 q (disj (noisy 'L (nevero)) (noisy 'R (== q 'R)))))
(show "T2 left infinite pause / answer-level disj" answers T2 #:limit 12)
(show "T2 left infinite pause / observable pauses" pause/disj T2)

;; T3: local disj is not enough when conj is composition (C6).
(defrel (counto n) (conde [(== n 'zero)] [(fresh (m) (== n (list 's m)) (counto m))]))
(define (T3) (run 1 q (counto q) (conde [(== q 'zero) (nevero)] [(== q '(s (s zero)))])))
(show "T3 conj = composition, second goal pauses forever on 1st answer" pause/disj T3 #:limit 16)
(show "T3 conj = append-map-inf" pause/full T3 #:limit 30)
