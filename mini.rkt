#lang typed/racket/base

(require "micro.rkt"
         (except-in racket/match ==)
         typed/amb)

(provide (all-from-out "micro.rkt")
         (all-defined-out))


(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (match (walk v s)
    [(cons v0 v1)
     (cons (walk* v0 s)
           (walk* v1 s))]
    [v v]))


(: reify-name (→ Index Symbol))
(define (reify-name n)
  (string->symbol (format "_.~a" n)))

(: reify-s (→ Term Substitution Substitution))
(define (reify-s v s)
  (match (walk v s)
    [(? var? v) (ext-s v (reify-name (size-s s)) s)]
    [(cons v0 v1) (reify-s v1 (reify-s v0 s))]
    [_ s]))

(: reify (→ Term Term))
(define (reify v) (walk* v (reify-s v empty-s)))


(: disji (→ Goal * Goal))
(define (disji . g*)
  (match (remq* (list fail) g*)
    ['() fail]
    [`(,g) g]
    [g*
     (define len (length g*))
     (λ (s/c)
       (define s*
         (for/vector #:length len
                     ([g (in-list g*)])
                     : (Sequenceof State)
           (in-amb/do (g s/c))))
       (let loop ([i : Natural 0])
         (define s (vector-ref s* i))
         (define s/c
           (or (for/or : (Option State) ([s/c : State s]) s/c)
               (amb)))
         (amb s/c (loop (if (= (- len i) 1) 0 (add1 i))))))]))
