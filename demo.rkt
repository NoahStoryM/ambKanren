;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper
#lang typed/racket/base

(require racket/match typed/amb)
(provide (all-defined-out))


(define-type Term
  (∪ Boolean Number Char Bytes String Keyword Null Symbol
     Var (Pair Term Term)))
(define-type Substitution (Listof (Pair Var Term)))
(define-type Goal (→ State State))

(struct var
  ([tag : Any])
  #:type-name Var
  #:transparent)

(struct state
  ([substitution : Substitution]
   [counter : Natural])
  #:type-name State
  #:transparent)


(: var=? (→ Var Var Boolean))
(define (var=? x1 x2) (equal? x1 x2))

(: empty-s Substitution)
(: size-s (→ Substitution Index))
(: ext-s (→ Var Term Substitution Substitution))
(define empty-s '())
(define (size-s s) (length s))
(define (ext-s x v s)
  (when (occurs? x v s) (amb))
  `([,x . ,v] . ,s))

(: occurs? (→ Var Term Substitution Boolean))
(define (occurs? x v s)
  (let ([v (walk v s)])
    (or (and (var? v)
             (var=? x v))
        (and (pair? v)
             (or (occurs? x (car v) s)
                 (occurs? x (cdr v) s))))))

(: walk (→ Term Substitution Term))
(define (walk v s)
  (cond
    [(and (var? v) (assoc v s var=?))
     => (match-λ [(cons _ v) (walk v s)])]
    [else v]))

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
  (match-define (state s c) s/c)
  (define g (f (var c)))
  (g (state s (add1 c))))

(: == (→ Term Term Goal))
(define ((== u v) s/c)
  (match-define (state s c) s/c)
  (state (unify u v s) c))

(: disj (→ Goal Goal Goal))
(: conj (→ Goal Goal Goal))
(define ((disj g1 g2) s/c) ((amb g1 g2) s/c))
(define ((conj g1 g2) s/c) (g2 (g1 s/c)))

(: fail Goal)
(: succeed Goal)
(define (fail s/c) (amb))
(define (succeed s/c) s/c)


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


(define-syntax-rule (Zzz g) (ann (λ (s/c) (g s/c)) Goal))

(define-syntax-rule (disj+ g1 g2) (disj (Zzz g1) (Zzz g2)))
(define-syntax-rule (conj+ g1 g2) (conj (Zzz g1) (Zzz g2)))

(define-syntax all
  (syntax-rules ()
    [(_) succeed]
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (conj (Zzz g0) (all g ...))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ () g ...) (all g ...)]
    [(_ (x0 x ...) g ...)
     (call/fresh
      (λ (x0)
        (fresh (x ...) g ...)))]))

(define-syntax conde
  (syntax-rules (else)
    [(_) fail]
    [(_ [else g ...]) (all g ...)]
    [(_ [g ...]) (all g ...)]
    [(_ [g ...] c ...)
     (disj+ (all g ...) (conde c ...))]))

(define-syntax-rule (run n^ (x) g* ...)
  (let ([n : Real (or n^ +inf.0)]
        [x (var 0)]
        [g (fresh (x) g* ...)]
        [s/c (state empty-s 0)])
    (for/list : (Listof Term)
              ([s/c (in-amb (g s/c))]
               [_ (in-range n)])
      (match-define (state s c) s/c)
      (reify (walk* x s)))))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))
