#lang typed/racket/base

;; The core of ambKanren: substitutions, unification, and goal
;; combinators built on amb.
;;
;; walk, unify, and reification are adapted
;; from microKanren, Copyright (C) 2013 Jason Hemann and Daniel P.
;; Friedman, MIT License (https://github.com/jasonhemann/microKanren);
;; streams are replaced by amb.  See NOTICE.

(require "private/types.rkt"
         (except-in racket/match ==)
         typed/amb)

(provide sequence->amb
         (all-from-out "private/types.rkt")
         empty-s size-s ext-s
         walk walk* == unify occurs?
         reify-name reify-s reify
         fail succeed list->amb)


(: list->amb (∀ (a) (→ (Listof a) a)))
(define (list->amb a*) (for/amb : a #:length (length a*) ([a a*]) a))

(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (eq? x1 x2))

(: empty-s Substitution)
(: s-empty? (→ Substitution Boolean))
(: s-has-key? (→ Var Substitution Boolean))
(: apply-s (→ Substitution Var Term))
(: size-s (→ Substitution Index))
(: ext-s (→ Var Term Substitution Substitution))
(define empty-s #hasheq())
(define (s-empty? s) (hash-empty? s))
(define (s-has-key? v s) (hash-has-key? s v))
(define (apply-s s v) (hash-ref s v))
(define (size-s s) (hash-count s))
(define (ext-s x v s)
  (when (occurs? x v s) (amb-fail))
  (hash-set s x v))

(: apply-goal (→ Goal Substitution Substitution))
(define (apply-goal g s) (g s))


(: walk (→ Term Substitution Term))
(define (walk v s)
  (if (and (var? v) (s-has-key? v s))
      (walk (apply-s s v) s)
      v))

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (match (walk v s)
    [(cons v0 v1)
     (cons (walk* v0 s)
           (walk* v1 s))]
    [v v]))


(: == (→ Term Term Goal))
(define ((== v w) s) (unify v w s))

(: unify (→ Term Term Substitution Substitution))
(define (unify v w s)
  (match* ((walk v s) (walk w s))
    [((? var? v) (? var? w)) #:when (var=? v w) s]
    [((? var? v) w) (ext-s v w s)]
    [(v (? var? w)) (ext-s w v s)]
    [((cons v0 v1) (cons w0 w1))
     (unify v1 w1 (unify v0 w0 s))]
    [(v w) #:when (equal? v w) s]
    [(_ _) (amb-fail)]))


(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (or (and (var? v)
             (var=? x v))
        (and (pair? v)
             (or (occurs? x (car v) s)
                 (occurs? x (cdr v) s))))))


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


(: fail Goal)
(: succeed Goal)
(define (fail _) (amb-fail))
(define (succeed s) s)
