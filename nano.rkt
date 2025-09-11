#lang typed/racket/base

(require "types.rkt"
         typed/racket/unsafe
         typed/amb
         typed/goto)

(provide (all-from-out "types.rkt")
         (all-defined-out))


(: sequence->amb (∀ (a) (→ (Sequenceof a) a)))
(define (sequence->amb a*) (for/amb : a ([a a*]) a))


(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (equal? x1 x2))


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


(: apply-goal (→ Goal State State))
(define (apply-goal g s/c) (g s/c))


(unsafe-require/typed "private/utils.rkt"
  [rotate-queue! (→ SequenceTop Void)])
(define rotate-tasks! rotate-queue!)
(define make-tasks (current-amb-maker))
(define tasks-length (current-amb-length))
(define tasks-add! (current-amb-pusher))
(define tasks-del! (current-amb-popper))
