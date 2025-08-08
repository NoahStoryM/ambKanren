#lang typed/racket/base

(require "types.rkt"
         racket/match
         racket/sequence
         typed/amb)

(provide (all-defined-out) (rename-out [== ≡]))

(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (= (var-counter x1) (var-counter x2)))

(: empty-s State)
(define empty-s (state #hash() 0))

(: walk (→ Term Substitution Term))
(define (walk u s) (or (and (var? u) (hash-ref s u #f)) u))

(: ext-s (→ Var Term Substitution Substitution))
(define (ext-s x v s) (hash-set s x v))


(: unify (→ Term Term Substitution Substitution))
(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (unify (cdr u) (cdr v) (unify (car u) (car v) s))]
      [(eqv? u v) s]
      [else (amb)])))


(: fail Goal)
(: succeed Goal)
(define (fail _) (amb))
(define (succeed s/c) s/c)


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


(define-syntax cond-aux
  (syntax-rules (else)
    [(_ disj) (disj)]
    [(_ disj [else g ...]) (conj g ...)]
    [(_ disj [g ...]) (conj g ...)]
    [(_ disj [g ...] c ...)
     (disj (conj g ...) (cond-aux disj c ...))]))
