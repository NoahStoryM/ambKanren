;; Adapted from microKanren-test.scm in
;; https://github.com/jasonhemann/microKanren
;; Copyright (C) 2013 Jason Hemann and Daniel P. Friedman.
;; MIT License; see NOTICE.  Variables, written #(n) there, are (var n).
;; Where the original takes the car or cdr of a stream, or forces it,
;; these tests look at the list of all states, or at its first state.

;; "second-set t1"
(check-equal?
  (car (take-all (λ () ((call/fresh (λ (q) (== q 5))) empty-state))))
  `(((,(var 0) . 5)) . 1))

;; "second-set t2"
(check-equal?
  (cdr (take-all (λ () ((call/fresh (λ (q) (== q 5))) empty-state))))
  '())

;; "second-set t3"
(check-equal?
  (car (take-all (λ () (a-and-b empty-state))))
  `(((,(var 1) . 5) (,(var 0) . 7)) . 2))

;; "second-set t3, take"
(check-equal?
  (take 1 (λ () (a-and-b empty-state)))
  `((((,(var 1) . 5) (,(var 0) . 7)) . 2)))

;; "second-set t4"
(check-equal?
  (cadr (take-all (λ () (a-and-b empty-state))))
  `(((,(var 1) . 6) (,(var 0) . 7)) . 2))

;; "second-set t5"
(check-equal?
  (cddr (take-all (λ () (a-and-b empty-state))))
  '())

;; "who cares"
(check-equal?
  (take 1 (λ () ((call/fresh (λ (q) (fives q))) empty-state)))
  `((((,(var 0) . 5)) . 1)))

;; "take 2 a-and-b stream"
(check-equal?
  (take 2 (λ () (a-and-b empty-state)))
  `((((,(var 1) . 5) (,(var 0) . 7)) . 2)
    (((,(var 1) . 6) (,(var 0) . 7)) . 2)))

;; "take-all a-and-b stream"
(check-equal?
  (take-all (λ () (a-and-b empty-state)))
  `((((,(var 1) . 5) (,(var 0) . 7)) . 2)
    (((,(var 1) . 6) (,(var 0) . 7)) . 2)))

;; "ground appendo"
(check-equal?
  (take 1 (λ () (ground-appendo empty-state)))
  `((((,(var 2) b) (,(var 1)) (,(var 0) . a)) . 3)))

;; "ground appendo2"
(check-equal?
  (take 1 (λ () (ground-appendo2 empty-state)))
  `((((,(var 2) b) (,(var 1)) (,(var 0) . a)) . 3)))

;; "appendo"
(check-equal?
  (take 2 (λ () (call-appendo empty-state)))
  `((((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 2) . ,(var 3)) (,(var 1))) . 4)
    (((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 2) . ,(var 6)) (,(var 5)) (,(var 3) ,(var 4) . ,(var 6)) (,(var 1) ,(var 4) . ,(var 5))) . 7)))

;; "appendo2"
(check-equal?
  (take 2 (λ () (call-appendo2 empty-state)))
  `((((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 2) . ,(var 3)) (,(var 1))) . 4) (((,(var 0) ,(var 1) ,(var 2) ,(var 3)) (,(var 3) ,(var 4) . ,(var 6)) (,(var 2) . ,(var 6)) (,(var 5)) (,(var 1) ,(var 4) . ,(var 5))) . 7)))

;; "reify-1st across appendo"
(check-equal?
  (map reify-1st (take 2 (λ () (call-appendo empty-state))))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

;; "reify-1st across appendo2"
(check-equal?
  (map reify-1st (take 2 (λ () (call-appendo2 empty-state))))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

;; "many non-ans"
(check-equal?
  (take 1 (λ () (many-non-ans empty-state)))
  `((((,(var 0) . 3)) . 1)))
