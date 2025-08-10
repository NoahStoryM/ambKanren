(check-equal?
 (run* (r)
   (fresh (y x)
     (== `(,x ,y) r)))
 '((_.0 _.1)))
(check-equal?
 (run* (r)
   (fresh (v w)
     (== (let ([x v] [y w]) `(,x ,y)) r)))
 '((_.0 _.1)))

(check-equal?
 (run* (r)
   (caro '(a c o r n) r))
 '(a))
(check-equal?
 (run* (q)
   (caro '(a c o r n) 'a)
   (== #t q))
 '(#t))
(check-equal?
 (run* (r)
   (fresh (x y)
     (caro `(,r ,y) x)
     (== 'pear x)))
 '(pear))
(check-equal?
 (run* (r)
   (fresh (x y)
     (caro '(grape raisin pear) x)
     (caro '((a) (b) (c)) y)
     (== (cons x y) r)))
 '((grape a)))

(check-equal?
 (run* (r)
   (fresh (v)
     (cdro '(a c o r n) v)
     (caro v r)))
 '(c))
(check-equal?
 (run* (r)
   (fresh (x y)
     (cdro '(grape raisin pear) x)
     (caro '((a) (b) (c)) y)
     (== (cons x y) r)))
 '(((raisin pear) a)))

(check-equal?
 (run* (q)
   (cdro '(a c o r n) '(c o r n))
   (== #t q))
 '(#t))
(check-equal?
 (run* (x)
   (cdro '(c o r n) `(,x r n)))
 '(o))

(check-equal?
 (run* (l)
   (fresh (x)
     (cdro l '(c o r n))
     (caro l x)
     (== 'a x)))
 '((a c o r n)))
(check-equal?
 (run* (l)
   (conso '(a b c) '(d e) l))
 '(((a b c) d e)))
