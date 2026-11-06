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
;; amb instead of streams.  The suspension that the second edition
;; inserts in every relation (an immature stream) is a choice point
;; whose continuation is sent to the back of amb's frontier.

(require (for-syntax racket/base syntax/parse)
         typed/amb)

(provide (all-defined-out))


;;; Terms and substitutions

(define-type Term
  (∪ Boolean Number Char Bytes String Keyword Null Symbol
     Var (Pair Term Term)))
(define-type Substitution (Immutable-HashTable Var Term))
(define-type Goal (→ Substitution Substitution))

(struct var ([name : Symbol])
  #:type-name Var
  #:transparent)

(: empty-s Substitution)
(define empty-s #hasheq())

(: walk (→ Term Substitution Term))
(define (walk v s)
  (if (and (var? v) (hash-has-key? s v))
      (walk (hash-ref s v) s)
      v))

(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eq? v x)]
      [(pair? v) (or (occurs? x (car v) s) (occurs? x (cdr v) s))]
      [else #f])))

;; Fails (with amb) instead of returning #f.
(: ext-s (→ Var Term Substitution Substitution))
(define (ext-s x v s)
  (if (occurs? x v s) (amb) (hash-set s x v)))

(: unify (→ Term Term Substitution Substitution))
(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(eq? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (unify (cdr u) (cdr v) (unify (car u) (car v) s))]
      [(equal? u v) s]
      [else (amb)])))


;;; Goals

(: == (→ Term Term Goal))
(define ((== u v) s) (unify u v s))

(: succeed Goal)
(define (succeed s) s)

(: fail Goal)
(define (fail s) (amb))

;; mplus: a choice between the two goals.
(: disj2 (→ Goal Goal Goal))
(define ((disj2 g1 g2) s) ((amb g1 g2) s))

;; bind: function composition.
(: conj2 (→ Goal Goal Goal))
(define ((conj2 g1 g2) s) (g2 (g1 s)))

;; The suspension of an immature stream: give up control and continue
;; after the computations already waiting in amb's frontier.  amb
;; places a new choice point according to the previous choice point's
;; `current-amb-depth-first?`, so the first, non-depth-first choice
;; point sends the second one, which is the rest of this computation,
;; to the back of the frontier.
(: suspend (→ Void))
(define (suspend)
  (parameterize ([current-amb-depth-first? #f])
    (amb (void)))
  (amb (void)))

(: call/fresh (→ Symbol (→ Var Goal) Goal))
(define (call/fresh name f) (f (var name)))

(: ifte (→ Goal Goal Goal Goal))
(define ((ifte g1 g2 g3) s)
  (define-values (more? get)
    (sequence-generate (in-amb/do (g1 s))))
  (if (more?)
      (g2 (let loop : Substitution ()
            (define s1 (cast (get) Substitution))
            (if (and (more?) (not (amb #t #f))) (loop) s1)))
      (g3 s)))

(: once (→ Goal Goal))
(define ((once g) s)
  (define-values (more? get)
    (sequence-generate (in-amb/do (g s))))
  (if (more?) (cast (get) Substitution) (amb)))


;;; Reification

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(: reify-name (→ Index Symbol))
(define (reify-name n)
  (string->symbol (format "_~a" n)))

(: reify-s (→ Term Substitution Substitution))
(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v) (hash-set r v (reify-name (hash-count r)))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) r))]
      [else r])))

(: reify (→ Term (→ Substitution Term)))
(define ((reify v) s)
  (let* ([v (walk* v s)]
         [r (reify-s v empty-s)])
    (walk* v r)))

(: run-goal (→ (Option Natural) Goal (Listof Substitution)))
(define (run-goal n g)
  (for/list : (Listof Substitution)
            ([_ (in-range (or n +inf.0))]
             [s : Substitution (in-amb/do (g empty-s))])
    s))


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
           (suspend)
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
