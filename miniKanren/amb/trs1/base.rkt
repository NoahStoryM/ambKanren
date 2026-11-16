#lang typed/racket/base

;; Substitutions, unification, and reification follow
;; typed/miniKanren/trs1; only failure uses amb-fail instead of #f.
;;
;; walk, unify, and reification are adapted
;; from microKanren, Copyright (C) 2013 Jason Hemann and Daniel P.
;; Friedman, MIT License (https://github.com/jasonhemann/microKanren);
;; streams are replaced by amb.  See NOTICE.

(require "private/types.rkt" typed/amb)

(provide sequence->amb
         (all-from-out "private/types.rkt")
         empty-s size-s ext-s
         walk walk* == unify occurs?
         reify-name reify-s reify
         fail succeed list->amb)


(: empty-s Substitution)
(: size-s (→ Substitution Index))
(: ext-s (case→
          (→ Var Data Substitution Substitution)
          (→ Var Term Substitution Substitution)))
(define empty-s '())
(define (size-s s) (length s))
(define (ext-s x v s)
  (if (or (data? v) (not (occurs? x v s)))
      (cons (cons x v) s)
      (amb-fail)))

(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (or (and (var? v)
             (eq? x v))
        (and (pair? v)
             (or (occurs? x (car v) s)
                 (occurs? x (cdr v) s))))))

(: walk (→ Term Substitution Term))
(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(assq v s)
        => (λ (a) (walk (cdr a) s))]
       [else v])]
    [else v]))

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
        (walk* (car v) s)
        (walk* (cdr v) s))]
      [else v])))

(: unify (→ Term Term Substitution Substitution))
(define (unify v w s)
  (let ([v (walk v s)] [w (walk w s)])
    (cond
      [(eq? v w) s]
      [(var? v) (ext-s v w s)]
      [(var? w) (ext-s w v s)]
      [(and (pair? v) (pair? w))
       (cond
         [(unify (car v) (car w) s)
          => (λ (s) (unify (cdr v) (cdr w) s))]
         [else (amb-fail)])]
      [(equal? v w) s]
      [else (amb-fail)])))

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


(: list->amb (∀ (a) (→ (Listof a) a)))
(define (list->amb a*) (for/amb : a #:length (length a*) ([a a*]) a))

(: == (→ Term Term Goal))
(define ((== v w) s) (unify v w s))

(: fail Goal)
(: succeed Goal)
(define (fail _) (amb-fail))
(define (succeed s) s)
