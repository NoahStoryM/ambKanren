#lang typed/racket/base

(require "private/types.rkt"
         racket/match
         racket/sequence
         typed/amb)

(provide (all-from-out "private/types.rkt")
         (all-defined-out) )

(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (= (var-counter x1) (var-counter x2)))

(: empty-s Substitution)
(: ext-s (→ Var Term Substitution Substitution))
(: size-s (→ Substitution Index))
(: in-s? (→ Var Substitution Boolean))
(: apply-s (→ Substitution Var Term))
(define empty-s #hash())
(define (ext-s x v s) (hash-set s x v))
(define (size-s s) (hash-count s))
(define (in-s? v s) (hash-has-key? s v))
(define (apply-s s v) (hash-ref s v))


(: walk (→ Term Substitution Term))
(define (walk v s)
  (if (and (var? v) (in-s? v s))
      (walk (apply-s s v) s)
      v))

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (if (pair? v)
        (cons (walk* (car v) s)
              (walk* (cdr v) s))
        v)))


(: unify (→ Term Term Substitution Substitution))
(define (unify v w s)
  (let ([v (walk v s)] [w (walk w s)])
    (cond
      [(and (var? v) (var? w) (var=? v w)) s]
      [(var? v) (ext-s v w s)]
      [(var? w) (ext-s w v s)]
      [(and (pair? v) (pair? w))
       (unify (cdr v) (cdr w) (unify (car v) (car w) s))]
      [(equal? v w) s]
      [else (amb)])))


(: reify-name (→ Index Symbol))
(define (reify-name n)
  (string->symbol (format "_.~a" n)))

(: reify-s (→ Term Substitution Substitution))
(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (ext-s v (reify-name (size-s s)) s)]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(: reify (→ Term Term))
(define (reify v) (walk* v (reify-s v empty-s)))


(: fail Goal)
(: succeed Goal)
(define (fail _) (amb))
(define (succeed s/c) (amb s/c))


(: disje (→ Goal * Goal))
(define (disje . g*)
  (match (remq* (list fail) g*)
    ['() fail]
    [`(,g) g]
    [g*
     (λ (s/c)
       ((for/amb : Goal
                 #:length (length g*)
                 #:fill fail
                 ([g (in-list g*)])
          g)
        s/c))]))

(: disji (→ Goal * Goal))
(define (disji . g*)
  (match (remq* (list fail) g*)
    ['() fail]
    [`(,g) g]
    [g*
     (define len (length g*))
     (λ (s/c)
       (define s*
         (for/vector #:length len #:fill empty-sequence
                     ([g (in-list g*)])
                     : (Sequenceof State)
           (in-amb/do (g s/c))))
       (let loop ([i : Natural 0])
         (define s (vector-ref s* i))
         (define s/c
           (or (for/or : (Option State) ([s/c : State s]) s/c)
               (amb)))
         (amb s/c (loop (if (= (- len i) 1) 0 (add1 i))))))]))

(: conj (→ Goal * Goal))
(define (conj . g*)
  (match (remq* (list succeed) g*)
    ['() succeed]
    [`(,g) g]
    [g* (λ (s/c) (for/fold ([s/c s/c]) ([g (in-list g*)]) (g s/c)))]))


(: call/fresh (→ (→ Var Goal) Goal))
(define ((call/fresh f) s/c)
  (match-define (state s c) s/c)
  (define g (f (var c)))
  (g (state s (add1 c))))
