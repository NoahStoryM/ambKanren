#lang typed/racket/base

(require racket/match typed/amb)
(provide (all-defined-out))

(define-type Term (∪ Boolean Complex Char Bytes String Keyword Null Symbol Var (Pair Term Term)))
(define-type Substitution (Listof (Pair Var Term)))
(define-type State (Pair Substitution Natural))
(define-type Goal (→ State State))

(struct var ([counter : Natural]) #:type-name Var #:transparent)
(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (= (var-counter x1) (var-counter x2)))

(: empty-s State)
(define empty-s (cons '() 0))

(: walk (→ Term Substitution Term))
(define (walk u s)
  (: pr (Option (Pair Var Term)))
  (define pr
    (and (var? u)
         (assf (λ ([v : Var]) (var=? u v)) s)))
  (if pr (walk (cdr pr) s) u))

(: ext-s (→ Var Term Substitution Substitution))
(define (ext-s x v s) `([,x . ,v] . ,s))


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


(: call/fresh (→ (→ Var Goal) Goal))
(define ((call/fresh f) s/c)
  (define s (car s/c))
  (define c (cdr s/c))
  (define g (f (var c)))
  (g (cons s (add1 c))))

(: list->amb (∀ (a) (→ (Listof a) a)))
(define list->amb
  (match-λ
    ['() (amb)]
    [`(,a) a]
    [`(,a . ,a*) (amb a (list->amb a*))]))

(: apply-goal (→ Goal State State))
(define (apply-goal g s/c) (g s/c))

(: disj (→ Goal * Goal))
(: conj (→ Goal * Goal))
(define ((disj . g*) s/c) ((list->amb g*) s/c))
(define ((conj . g*) s/c) (foldl apply-goal s/c g*))


(: fail Goal)
(: succeed Goal)
(define (fail _) (amb))
(define (succeed s/c) s/c)

(: == (→ Term Term Goal))
(define ((== u v) s/c)
  (define c (cdr s/c))
  (define s (unify u v (car s/c)))
  (cons s c))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...)
     (conj g ...)]
    [(_ (x x* ...) g ...)
     (call/fresh (λ (x) (fresh (x* ...) g ...)))]))

(define-syntax conde
  (syntax-rules (else)
    [(_) fail]
    [(_ [else g ...]) (conj g ...)]
    [(_ [g ...]) (conj g ...)]
    [(_ [g ...] c ...)
     (disj (conj g ...) (conde c ...))]))

(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real n^] [g (fresh (x) g* ...)])
    (for/list : (Listof Term)
              ([t (in-amb (walk (var 0) (car (g empty-s))))]
               [_ (in-range n)])
      t)))
(define-syntax-rule (run* (x) g* ...) (run +inf.0 (x) g* ...))
