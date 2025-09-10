#lang typed/racket/base

(require "micro.rkt"
         (except-in racket/match ==)
         typed/amb
         typed/data/queue)

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
     (λ (s/c)
       (: q (Queue (Sequenceof State) (Sequenceof State)))
       (define q (make-queue))
       (for ([g (in-list g*)])
         (enqueue! q (in-amb/do (g s/c))))
       (for*/amb : State
                 ([_ (in-naturals)]
                  #:break (queue-empty? q)
                  [s/c* (in-value (dequeue! q))]
                  [s/c (in-value (for/or : (Option State) ([s/c : State s/c*]) s/c))]
                  #:when s/c)
         (enqueue! q s/c*)
         s/c))]))

(: conji (→ Goal * Goal))
(define (conji . g*)
  (if (memq fail g*)
      fail
      (match (remq* (list succeed) g*)
        ['() succeed]
        [`(,g) g]
        [g*
         (λ (s/c)
           (define s/c*
             (parameterize ([current-amb-maker make-tasks]
                            [current-amb-length tasks-length]
                            [current-amb-pusher tasks-add!]
                            [current-amb-popper tasks-del!])
               (for/fold : (Sequenceof State)
                         ([s/c* (in-value s/c)])
                         ([g (in-list g*)]
                          [i (in-naturals)])
                 (in-amb/do
                  (let* ([s/c (sequence->amb s/c*)]
                         [s/c* (in-amb/do (g s/c))])
                    (parameterize ([current-amb-rotator rotate-tasks!])
                      (sequence->amb s/c*)))))))
           (sequence->amb s/c*))])))
