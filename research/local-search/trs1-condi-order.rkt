#lang typed/racket/base

;; Side check: 3-clause condi answer order, TRS1 book implementation
;; (miniKanren-typed) vs ambKanren TRS1 (n-ary round-robin mplusi).
;; Run: racket trs1-condi-order.rkt

(require (prefix-in B: typed/miniKanren/trs1)
         (prefix-in A: typed/miniKanren/amb/trs1))

(define-syntax-rule (prog run condi conde == q)
  (run #f (q)
    (condi
      [(conde [(== q 'a1)] [(== q 'a2)] [(== q 'a3)])]
      [(conde [(== q 'b1)] [(== q 'b2)] [(== q 'b3)])]
      [else (conde [(== q 'c1)] [(== q 'c2)] [(== q 'c3)])])))

(printf "book TRS1 : ~s\n" (prog B:run B:condi B:conde B:== q))
(printf "ambKanren : ~s\n" (prog A:run A:condi A:conde A:== q))
