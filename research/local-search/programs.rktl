;; Example programs, written in the common TRS2 surface language so the
;; same text is `include`d by the untyped kernel (all schemes) and by a
;; typed module using miniKanren-typed TRS2 (the reference).

(defrel (nevero) (nevero))
(defrel (alwayso) (conde [succeed] [(alwayso)]))
(defrel (anyo [g : Goal]) (conde [g] [(anyo g)]))
(defrel (deepo n)                      ; n nested suspensions, then succeed
  (conde [(== n '())]
         [(fresh (m) (== n (cons 'z m)) (deepo m))]))
(defrel (appendo l t out)
  (conde
    [(== l '()) (== t out)]
    [(fresh (a d res)
       (== (cons a d) l)
       (== (cons a res) out)
       (appendo d t res))]))
(defrel (counto n)                     ; 0, (s 0), (s (s 0)), ...
  (conde [(== n 'zero)]
         [(fresh (m) (== n (list 's m)) (counto m))]))

(define programs
  (list
   ;; finite, flat and nested disj: answer ORDER is the point
   (cons 'finite-nested-disj
         (λ () (run* q (disj (== q 'a) (disj (== q 'b) (== q 'c))))))
   (cons 'finite-disj-under-conj
         (λ () (run* (x y)
                 (conde [(== x 1)] [(== x 2)])
                 (conde [(== y 'a)] [(== y 'b)]))))
   ;; C1: left produces infinitely many answers, right succeeds at once
   (cons 'C1-left-infinite-answers
         (λ () (run 4 q (conde [(alwayso) (== q 'L)] [(== q 'R)]))))
   ;; C2: left pauses forever without answers, right succeeds at once
   (cons 'C2-left-infinite-pause
         (λ () (run 1 q (conde [(nevero)] [(== q 'R)]))))
   (cons 'C2b-left-infinite-pause-right-later
         (λ () (run 1 q (conde [(nevero)] [(deepo '(z z z)) (== q 'R)]))))
   ;; C3: both sides suspend, different depths
   (cons 'C3-nested-suspensions
         (λ () (run* q (conde [(deepo '(z z z)) (== q 'deep3)]
                              [(deepo '(z)) (== q 'deep1)]
                              [(== q 'now)]))))
   ;; C4: conj over a disj whose left branch never answers
   (cons 'C4-conj-of-disj-with-nevero
         (λ () (run 2 q (conde [(nevero)] [succeed])
                        (conde [(== q 1)] [(== q 2)]))))
   ;; C5: disj nesting conj; the conj's head never answers
   (cons 'C5-disj-of-conj-with-nevero
         (λ () (run 1 q (conde [(nevero) (== q 1)] [(== q 2)]))))
   ;; C6: conj whose FIRST goal has infinitely many answers and whose
   ;; second goal pauses forever on the first of them
   (cons 'C6-conj-infinite-left-then-nevero
         (λ () (run 1 q (counto q)
                        (conde [(== q 'zero) (nevero)]
                               [(== q '(s (s zero)))]))))
   ;; C7: conj: first goal infinitely many answers; second fails on all
   ;; but one which comes late
   (cons 'C7-recursive-generate-and-test
         (λ () (run 1 q (counto q) (== q '(s (s (s zero)))))))
   ;; recursive relations
   (cons 'R1-appendo-split
         (λ () (run* (x y) (appendo x y '(1 2 3)))))
   (cons 'R2-appendo-open
         (λ () (run 5 (x y z) (appendo x y z))))
   (cons 'R3-anyo-disj
         (λ () (run 5 q (anyo (conde [(== q 1)] [(== q 2)])))))
   ))
