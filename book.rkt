#lang racket/base

(require (rename-in racket/base
                    [λ λF] [λ λG]
                    [car lhs] [cdr rhs] [length size-s]
                    [box-immutable var] [box? var?]))
(provide (all-defined-out)
         λF λG
         lhs rhs size-s
         var var?)

(define empty-s '())
(define (ext-s x v s) (cons (cons x v) s))

(define (walk v s)
  (cond
    [(var? v)
     (cond
       [(assq v s)
        => (λ (a) (walk (rhs a) s))]
       [else v])]
    [else v]))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
        (walk* (car v) s)
        (walk* (cdr v) s))]
      [else v])))

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
         [else #f])]
      [(equal? v w) s]
      [else #f])))

(define (reify-name n)
  (string->symbol
   (string-append
    "_." (number->string n))))
(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (ext-s v (reify-name (size-s s)) s)]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))
(define (reify v) (walk* v (reify-s v empty-s)))


(define-syntax-rule (run n^ (x) g ...)
  (let ([n n^] [x (var 'x)])
    (if (or (not n) (> n 0))
        (map∞ n
              (λ (s) (reify (walk* x s)))
              ((all g ...) empty-s))
        '())))
(define-syntax-rule (run* (x) g* ...) (run #f (x) g* ...))

(define-syntax-rule
  (case∞ e
    [() on-zero]
    [(a^) on-one]
    [(a f) on-choice])
  (let ([a∞ e])
    (cond
      [(not a∞)
       on-zero]
      [(not (and (pair? a∞) (procedure? (cdr a∞))))
       (let ([a^ a∞])
         on-one)]
      [else
       (let ([a (car a∞)] [f (cdr a∞)])
         on-choice)])))

(define-syntax-rule (mzero) #f)
(define-syntax-rule (unit a) a)
(define-syntax-rule (choice a f) (cons a f))

(define (map∞ n p a∞)
  (case∞ a∞
    [() '()]
    [(a) (list (p a))]
    [(a f)
     (cons
      (p a)
      (cond
        [(not n) (map∞ n p (f))]
        [(> n 1) (map∞ (sub1 n) p (f))]
        [else '()]))]))

(define succeed (λG (s) (unit s)))
(define fail (λG (s) (mzero)))

(define (== v w)
  (λG (s)
    (cond
      [(unify v w s) => succeed]
      [else (fail s)])))

(define-syntax-rule (fresh (x ...) g ...)
  (λG (s)
    (let ([x (var 'x)] ...)
      ((all g ...) s))))

(define-syntax cond-aux
  (syntax-rules (else)
    [(_ ifer) fail]
    [(_ ifer [else g ...]) (all g ...)]
    [(_ ifer [g ...]) (all g ...)]
    [(_ ifer [g0 g ...] c ...)
     (ifer g0 (all g ...) (cond-aux ifer c ...))]))
(define-syntax-rule (conde c ...) (cond-aux ife c ...))
(define-syntax-rule (condi c ...) (cond-aux ifi c ...))
(define-syntax-rule (conda c ...) (cond-aux ifa c ...))
(define-syntax-rule (condu c ...) (cond-aux ifu c ...))

(define-syntax all-aux
  (syntax-rules ()
    [(_ bnd) succeed]
    [(_ bnd g) g]
    [(_ bnd g0 g ...)
     (let ([g^ g0])
       (λG (s)
         (bnd (g^ s)
              (λG (s)
                ((all-aux bnd g ...) s)))))]))
(define-syntax-rule (all  g ...) (all-aux bind  g ...))
(define-syntax-rule (alli g ...) (all-aux bindi g ...))


(define (mplus a∞ f)
  (case∞ a∞
    [() (f)]
    [(a) (choice a f)]
    [(a f0) (choice a (λF () (mplus (f0) f)))]))

(define (bind a∞ g)
  (case∞ a∞
    [() (mzero)]
    [(a) (g a)]
    [(a f) (mplus (g a) (λF () (bind (f) g)))]))

(define (mplusi a∞ f)
  (case∞ a∞
    [() (f)]
    [(a) (choice a f)]
    [(a f0) (choice a (λF () (mplusi (f) f0)))]))

(define (bindi a∞ g)
  (case∞ a∞
    [() (mzero)]
    [(a) (g a)]
    [(a f) (mplusi (g a) (λF () (bindi (f) g)))]))

(define-syntax-rule (ife g0 g1 g2)
  (λG (s) (mplus ((all g0 g1) s) (λF () (g2 s)))))

(define-syntax-rule (ifi g0 g1 g2)
  (λG (s) (mplusi ((all g0 g1) s) (λF () (g2 s)))))

(define-syntax-rule (ifa g0 g1 g2)
  (λG (s)
    (let ([s∞ (g0 s)])
      (case∞ s∞
        [() (g2 s)]
        [(s) (g1 s)]
        [(s f) (bind s∞ g1)]))))

(define-syntax-rule (ifu g0 g1 g2)
  (λG (s)
    (let ([s∞ (g0 s)])
      (case∞ s∞
        [() (g2 s)]
        [(s) (g1 s)]
        [(s f) (g1 s)]))))
