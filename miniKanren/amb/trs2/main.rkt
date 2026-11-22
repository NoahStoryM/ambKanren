#lang typed/racket/base

;; ambKanren with the language of The Reasoned Schemer, second edition.
;;
;; The interface and the definitions of walk, unify, ifte, once, and
;; reification follow the official code of the second edition
;; (https://github.com/TheReasonedSchemer2ndEd/CodeFromTheReasonedSchemer2ndEd,
;; trs2-impl.scm), Copyright (c) 2018 Daniel P. Friedman, William E.
;; Byrd, Oleg Kiselyov, and Jason Hemann, MIT License; see NOTICE.
;;
;; Goals are Substitution -> Substitution, and backtracking is done by
;; amb instead of streams.  Each relation explicitly pauses; the merger
;; of its choices determines the scheduling order.

(require (for-syntax racket/base syntax/parse)
         typed/amb
         "../../../private/search.rkt")

(provide (all-defined-out))


;;; Terms and substitutions

(define-type Literal (∪ Boolean Number Char Bytes String Keyword Null Symbol))
(define-type Data (∪ Literal (Pair Data Data)))
(define-type Term (∪ Literal Var (Pair Term Term)))
(define-type Substitution (Listof (Pair Var Term)))

(struct var ([name : Symbol])
  #:type-name Var
  #:transparent)

(define-predicate data? Data)

(: empty-s Substitution)
(define empty-s '())

(: walk (→ Term Substitution Term))
(define (walk v s)
  (cond
    [(and (var? v) (assq v s))
     => (λ (a) (walk (cdr a) s))]
    [else v]))

;; As in the reference, ground terms skip the occurs check.
;; Failure invokes amb-fail, so the result type has no Option.
(: ext-s (case→
          (→ Var Data Substitution Substitution)
          (→ Var Term Substitution Substitution)))
(define (ext-s x v s)
  (cond
    [(data? v) (cons (cons x v) s)]
    [(occurs? x v s) (amb-fail)]
    [else (cons (cons x v) s)]))

(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
      [else #f])))

(: unify (→ Term Term Substitution Substitution))
(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(eq? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))]
      [(equal? u v) s]
      [else (amb-fail)])))


(define-type Goal (→ Substitution Substitution))

;;; Goals

(: == (→ Term Term Goal))
(define ((== u v) s) (unify u v s))

(: succeed Goal)
(define (succeed s) s)

(: fail Goal)
(define (fail s) (amb-fail))

;; mplus: a choice between the two goals.
(: disj2 (→ Goal Goal Goal))
(define ((disj2 g1 g2) s) ((amb g1 g2) s))

;; bind: g2 continues from each answer of a local search of g1, so
;; the answers of g1 are one choice, merged in the order of the answers
;; (append-map-inf), not in the shape of the choices inside g1.
(: conj2 (→ Goal Goal Goal))
(define ((conj2 g1 g2) s) (g2 (amb-local (g1 s))))

(: call/fresh (→ Symbol (→ Var Goal) Goal))
(define (call/fresh name f)
  (f (var name)))

(: ifte (→ Goal Goal Goal Goal))
(define ((ifte g1 g2 g3) s)
  (define-values (v next) (first-answer (λ () (g1 s))))
  (if v
      (g2 (make-amb (λ () (values v next))))
      (g3 s)))

(: once (→ Goal Goal))
(define ((once g) s)
  (define-values (v next) (first-answer (λ () (g s))))
  (if v (car v) (amb-fail)))


;;; Reification

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons (walk* (car v) s)
             (walk* (cdr v) s))]
      [else v])))

(: reify-name (→ Index Symbol))
(define (reify-name n)
  (string->symbol (format "_~a" n)))

;; The names are symbols, hence ground, so this ext-s cannot fail.
(: reify-s (→ Term Substitution Substitution))
(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v) (ext-s v (reify-name (length r)) r)]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) r))]
      [else r])))

(: reify (→ Term (→ Substitution Term)))
(define ((reify v) s)
  (let* ([v (walk* v s)]
         [r (reify-s v empty-s)])
    (walk* v r)))

(: run-goal (→ (Option Natural) Goal (Listof Substitution)))
(define (run-goal n g)
  (take-answers n (λ () (g empty-s))))


;;; Syntax

(define-syntax disj
  (syntax-rules ()
    [(_) fail]
    [(_ g) g]
    [(_ g0 g ...) (disj2 g0 (disj g ...))]))

(define-syntax conj
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g0 g ...) (conj2 g0 (conj g ...))]))

(define-syntax (defrel stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(_ (name:id arg ...) g ...)
     #:with (param ...)
     (for/list ([arg (in-list (syntax->list #'(arg ...)))])
       (syntax-parse arg
         #:datum-literals (:)
         [x:id #'[x : Term]]
         [[x:id : t] arg]))
     #'(define (name param ...) : Goal
         (λ (s)
           (amb-pause)
           ((conj g ...) s)))]))

(define-syntax run
  (syntax-rules ()
    [(_ n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                (== (list x0 x ...) q)
                g ...))]
    [(_ n q g ...)
     (let ([q (var 'q)])
       (map (reify q) (run-goal n (conj g ...))))]))

(define-syntax-rule (run* q g ...) (run #f q g ...))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (conj g ...)]
    [(_ (x0 x ...) g ...)
     (call/fresh 'x0
                 (λ ([x0 : Var])
                   (fresh (x ...) g ...)))]))

(define-syntax-rule (conde [g ...] ...)
  (disj (conj g ...) ...))

(define-syntax conda
  (syntax-rules ()
    [(_ [g0 g ...]) (conj g0 g ...)]
    [(_ [g0 g ...] ln ...)
     (ifte g0 (conj g ...) (conda ln ...))]))

(define-syntax-rule (condu [g0 g ...] ...)
  (conda [(once g0) g ...] ...))

(define-syntax-rule (project (x ...) g ...)
  (λ ([s : Substitution])
    (let ([x (walk* x s)] ...)
      ((conj g ...) s))))
