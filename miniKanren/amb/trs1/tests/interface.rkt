#lang typed/racket/base

(require (except-in typed/rackunit fail)
         typed/miniKanren/amb/trs1)

;; Chapter 9's project is an exercise, not part of the library interface.
(define-syntax project
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x ...) g ...)
     (λG (s) (let ([x (walk* x s)] ...) ((all g ...) s)))]))

(check-equal?
 (run* (q)
   (== #f q)
   (project (q)
     (== (not (not q)) q)))
 '(#f))

;; The same type aliases and predicate as miniKanren-typed, including
;; occurrence typing for a recursive pair that contains no variables.
(: literal Literal)
(: datum Data)
(define literal 'leaf)
(define datum (cons literal '(#t 3 #\x #"bytes" "text" #:key ())))
(check-true (data? datum))
(check-false (data? (var 'x)))
(check-false (data? (cons 'a (var 'x))))
(check-false (data? (vector 'a)))

(: as-data (→ Any (Option Data)))
(define (as-data v) (and (data? v) v))
(check-equal? (as-data datum) datum)

;; all-aux now accepts a binding operator, not a conjunction form.
(: direct-bind (→ Substitution Goal Substitution))
(define (direct-bind s g) (g s))
(check-equal? ((all-aux direct-bind) empty-s) empty-s)
(define identity-goal (λG (s) s))
(check-true (eq? (all-aux direct-bind identity-goal) identity-goal))
(check-true (eq? (all identity-goal) identity-goal))
(check-true (eq? (alli identity-goal) identity-goal))
(check-equal?
 (run* (q)
   (all-aux direct-bind (== q 'answer) succeed))
 '(answer))

;; As in the reference, construct the first goal once, when all-aux
;; is evaluated; construct the remaining goal when it is applied.
(define built : (Listof Symbol) '())
(: marked (→ Symbol Goal))
(define (marked tag)
  (set! built (append built (list tag)))
  succeed)
(define g (all-aux direct-bind (marked 'first) (marked 'rest)))
(check-equal? built '(first))
(check-equal? (g empty-s) empty-s)
(check-equal? built '(first rest))
(check-equal? (g empty-s) empty-s)
(check-equal? built '(first rest rest))

(test-case "fresh with no variables delays its goals"
  (check-equal? (run* (q) fail (fresh () (error 'early "forced"))) '())
  (check-equal? (run* (q) (fresh () (== q 'answer))) '(answer)))
