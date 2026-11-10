#lang typed/racket/base

;; The search of typed/miniKanren/amb/trs2: fairness of ifte and once,
;; and laws of the search that the book's streams do not obey.

(require (except-in typed/rackunit fail)
         "../main.rkt")

(defrel (nevero) (nevero))
(defrel (skip) succeed)

;; ifte and once: a test that keeps pausing does not block the search
(check-equal? (run 1 q (conde [(once (nevero))] [(== q 1)])) '(1))
(check-equal? (run 1 q (conde [(conda [(nevero) succeed] [succeed succeed])] [(== q 1)])) '(1))

;; the pauses of a test count as pauses of the goal that runs it
(defrel (oneo q) (== q 'A))
(check-equal? (run* q (disj (once (oneo q)) (== q 'B))) '(B A))

;; ifte runs the else branch only when the test has no answer at all
(defrel (late-failo) fail)
(check-equal? (run* q (ifte (late-failo) succeed (== q 'ok))) '(ok))
(check-equal? (run* q (conda [(conde [(== q 1)] [(== q 2)])] [(== q 3)])) '(1 2))
(check-equal? (run* q (condu [(conde [(== q 1)] [(== q 2)])] [(== q 3)])) '(1))

;; laws: k(a) pauses once more than k(b) and k(c).  With the book's
;; streams the two sides of each law give different orders.
(defrel (k q r)
  (conde
    [(== q 'a) (skip) (== r 'A)]
    [(== q 'b) (== r 'B)]
    [(== q 'c) (== r 'C)]))
(define (x [q : Term]) (disj (== q 'a) (== q 'b)))
(define (y [q : Term]) (== q 'c))
(define (p [i : Term]) (disj (== i 0) (== i 1)))
(define (f [i : Term] [q : Term]) (conde [(== i 0) (x q)] [(== i 1) (y q)]))

;; conjunction distributes over disjunction
(check-equal? (run* r (fresh (q) (disj (x q) (y q)) (k q r)))
              (run* r (disj (fresh (q) (x q) (k q r)) (fresh (q) (y q) (k q r)))))
;; conjunction is associative
(check-equal? (run* r (fresh (i q) (fresh () (p i) (f i q)) (k q r)))
              (run* r (fresh (i q) (p i) (fresh () (f i q) (k q r)))))
;; fail is a unit of disjunction
(check-equal? (run* r (fresh (q) (disj fail (x q)) (k q r)))
              (run* r (fresh (q) (x q) (k q r))))
