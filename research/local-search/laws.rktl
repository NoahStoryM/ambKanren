;; Astra's counterexample (algebraic-control-flow-math #68) as goals.
;; k(a) pauses once more than k(b), k(c).
(defrel (skip) succeed)
(defrel (k q r)
  (conde
    [(== q 'a) (skip) (== r 'A)]
    [(== q 'b) (== r 'B)]
    [(== q 'c) (== r 'C)]))
(define (x [q : Term]) (disj (== q 'a) (== q 'b)))
(define (y [q : Term]) (== q 'c))
;; p = [0,1], f(0) = x, f(1) = y
(define (p [i : Term]) (disj (== i 0) (== i 1)))
(define (f [i : Term] [q : Term]) (conde [(== i 0) (x q)] [(== i 1) (y q)]))

(define laws
  (list
   (list "distributivity: (x ⊞ y) >>= k"
         (run* r (fresh (q) (disj (x q) (y q)) (k q r))))
   (list "distributivity: (x >>= k) ⊞ (y >>= k)"
         (run* r (disj (fresh (q) (x q) (k q r)) (fresh (q) (y q) (k q r)))))
   (list "associativity: (p >>= f) >>= k"
         (run* r (fresh (i q) (fresh () (p i) (f i q)) (k q r))))
   (list "associativity: p >>= (λi. f i >>= k)"
         (run* r (fresh (i q) (p i) (fresh () (f i q) (k q r)))))))
(for ([l (in-list laws)]) (printf "  ~a  ~s\n" (car l) (cadr l)))
