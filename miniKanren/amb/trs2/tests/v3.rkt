#lang typed/racket/base
(require (except-in typed/rackunit fail) typed/amb typed/amb/merge "../main.rkt")

(defrel (nevero) (nevero))
(test-case "committed choice forwards pauses to its enclosing search"
  (parameterize ([current-amb-merger amb-interleave])
    (check-equal? (run 1 q (conde [(once (nevero))] [(== q 'ready)])) '(ready))))

(test-case "run zero never starts its goal"
  (check-equal? (run-goal 0 (λ ([s : Substitution]) (error "early goal"))) '()))

(test-case "once fails on no answers and keeps just the first answer"
  (parameterize ([current-amb-merger amb-interleave])
    (check-equal? (run* q (once fail)) '())
    (check-equal? (run* q (once (conde [(== q 1)] [(== q 2)]))) '(1))))
