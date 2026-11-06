#lang typed/racket/base

;; microKanren, with streams replaced by amb.
;;
;; Source: https://github.com/jasonhemann/microKanren -- microKanren.scm
;; (the core) and miniKanren-wrappers.scm (the user-level forms), from
;; Jason Hemann and Daniel P. Friedman, "μKanren: A Minimal Functional
;; Core for Relational Programming", Scheme Workshop 2013.
;; Copyright (C) 2013 Jason Hemann and Daniel P. Friedman.  MIT License;
;; see NOTICE for the full text and for the changes made here.
;;
;; This follows the Typed Racket rewrite typed/microKanren of the
;; miniKanren-typed package, except that a goal returns one state
;; instead of a stream of states: it chooses among its answers with
;; amb, and fails with (amb).  The stream operations (unit, mzero,
;; mplus, bind, pull) are therefore gone.

(require typed/amb)
(provide (all-defined-out))


;;; Terms, substitutions, and states

(define-type Literal (∪ Boolean Number Char Bytes String Keyword Null Symbol))
(define-type Term (∪ Literal Var (Pair Term Term)))
(define-type Substitution (Listof (Pair Var Term)))
;; A state is a substitution and the counter for the next fresh variable.
(define-type State (Pair Substitution Natural))

;; A variable is identified by its counter.
(struct var ([c : Natural])
  #:type-name Var
  #:transparent)

(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (= (var-c x1) (var-c x2)))

(: walk (→ Term Substitution Term))
(define (walk u s)
  (let ([pr (and (var? u) (assf (λ ([v : Var]) (var=? u v)) s))])
    (if pr (walk (cdr pr) s) u)))

(: ext-s (→ Var Term Substitution Substitution))
(define (ext-s x v s) (cons (cons x v) s))


;;; Goals

;; A goal returns one of its states; the others are produced by
;; backtracking into the choices it made with amb.
(define-type Goal (→ State State))

(: == (→ Term Term Goal))
(define ((== u v) s/c)
  (let ([s (unify u v (car s/c))])
    (if s (cons s (cdr s/c)) (amb))))

(: unify (→ Term Term Substitution (Option Substitution)))
(define (unify u v s)
  (let ([u (walk u s)] [v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))]
      [else (and (equal? u v) s)])))

(: call/fresh (→ (→ Var Goal) Goal))
(define ((call/fresh f) s/c)
  (let ([c (cdr s/c)])
    ((f (var c)) (cons (car s/c) (+ c 1)))))

(: disj (→ Goal Goal Goal))
(define ((disj g1 g2) s/c) (amb (g1 s/c) (g2 s/c)))

(: conj (→ Goal Goal Goal))
(define ((conj g1 g2) s/c) (g2 (g1 s/c)))


;;; User-level forms (miniKanren-wrappers.scm)

;; Moves the rest of the computation to the back of the search
;; frontier, as returning an immature stream does in microKanren: the
;; first choice point is explored breadth-first, so the second one,
;; whose only alternative is the rest of the computation, is reached
;; only after the choices already waiting.
(: suspend (→ Void))
(define (suspend)
  (parameterize ([current-amb-depth-first? #f])
    (amb (void)))
  (amb (void)))

;; Inverse-η-delay: suspends before running the goal, so that a
;; recursive relation gives the other choices a turn instead of looping.
(define-syntax-rule (Zzz g)
  (λ ([s/c : State]) (suspend) (g s/c)))

(define-syntax conj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (conj (Zzz g0) (conj+ g ...))]))

(define-syntax disj+
  (syntax-rules ()
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (disj (Zzz g0) (disj+ g ...))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g0 g ...) (conj+ g0 g ...)]
    [(_ (x0 x ...) g0 g ...)
     (call/fresh
      (λ ([x0 : Var])
        (fresh (x ...) g0 g ...)))]))

(define-syntax-rule (conde [g0 g ...] ...)
  (disj+ (conj+ g0 g ...) ...))

(define-syntax-rule (run n (x ...) g0 g ...)
  (map reify-1st (take n (λ () (call/goal (fresh (x ...) g0 g ...))))))

(define-syntax-rule (run* (x ...) g0 g ...)
  (map reify-1st (take-all (λ () (call/goal (fresh (x ...) g0 g ...))))))

(: empty-state State)
(define empty-state (cons '() 0))

(: call/goal (→ Goal State))
(define (call/goal g) (g empty-state))

;; Collects the results of a nondeterministic computation.
(: take-all (∀ (a) (→ (→ a) (Listof a))))
(define (take-all th)
  (for/list : (Listof a) ([x : a (in-amb/do (th))]) x))

(: take (∀ (a) (→ Natural (→ a) (Listof a))))
(define (take n th)
  (for/list : (Listof a)
            ([_ (in-range n)]          ; checked first, so that no
             [x : a (in-amb/do (th))]) ; further answer is searched for
    x))

;; The value of the first variable, with its fresh variables reified.
(: reify-1st (→ State Term))
(define (reify-1st s/c)
  (let ([v (walk* (var 0) (car s/c))])
    (walk* v (reify-s v '()))))

(: walk* (→ Term Substitution Term))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(: reify-s (→ Term Substitution Substitution))
(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v)
       (let ([n (reify-name (length s))])
         (cons (cons v n) s))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))

(: reify-name (→ Natural Symbol))
(define (reify-name n)
  (string->symbol (string-append "_" "." (number->string n))))

;; Applies f to n fresh variables.
(: fresh/nf (→ Natural (→ Var * Goal) Goal))
(define (fresh/nf n f)
  (let app-f/v* ([n : Natural n] [v* : (Listof Var) '()])
    (cond
      [(zero? n) (apply f (reverse v*))]
      [else (call/fresh
             (λ ([x : Var])
               (app-f/v* (- n 1) (cons x v*))))])))
