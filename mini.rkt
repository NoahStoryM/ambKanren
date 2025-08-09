#lang typed/racket/base

(require "micro.rkt"
         (except-in racket/match ==)
         racket/sequence
         typed/amb)

(provide (all-from-out "micro.rkt")
         (all-defined-out))


(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (if (pair? v)
        (cons (walk* (car v) s)
              (walk* (cdr v) s))
        v)))


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
