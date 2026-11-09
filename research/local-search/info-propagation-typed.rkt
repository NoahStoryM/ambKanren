#lang typed/racket/base

;; Experiment A (Typed Racket): the same propagation facts via typed/amb.
;; Run: racket research/local-search/info-propagation-typed.rkt

(require typed/amb typed/rackunit)

(: tagged (→ Symbol (Listof Integer) (List Integer Any)))
(define (tagged tag xs)
  (define pending xs)
  (make-amb (λ () (pair? pending))
            (λ () (define v (car pending)) (set! pending (cdr pending))
               (list v (current-amb-info)))
            tag))

(define p : (Parameterof Integer) (make-parameter 0))

(check-equal?
 (for/list : (Listof Any)
           ([v (in-amb/do (amb (tagged 'L '(1 2)) (tagged 'R '(10))))])
   v)
 '((1 L) (2 L) (10 R)))

;; typed side reproduces the untyped A3 limitation
(check-equal?
 (for/list : (Listof Any)
           ([v (in-amb/do (parameterize ([p 1]) (tagged 'node '(1 2))))])
   v)
 '((1 #f) (2 #f)))

;; nested typed in-amb/do sees #f
(check-equal?
 (for/list : (Listof Any)
           ([v (in-amb/do
                (let ([x (tagged 'outer '(0))])
                  (define-values (more? get)
                    (sequence-generate (in-amb/do (current-amb-info))))
                  (and (more?) (get))))])
   v)
 '(#f))

(displayln "typed info-propagation: 3 checks done")
