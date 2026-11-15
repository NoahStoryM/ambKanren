#lang typed/racket/base

;; Search operations shared by the four languages.  Pauses remain
;; scheduling events until run collects the answers.
(require typed/amb/base)
(provide take-answers first-answer)

(: take-answers (∀ (a) (→ (Option Natural) (→ a) (Listof a))))
(define (take-answers n thk)
  (if (eqv? n 0)
      '()
      (let loop ([gen : (Generatorof (List a)) (λ () ((inst amb-generate* a) thk))]
                 [n : (Option Natural) n])
        (define-values (v next) (gen))
        (cond
          [(not v) '()]
          [(exact-positive-integer? v) (loop next n)]
          [else (cons (car v)
                      (if (eqv? n 1) '() (loop next (and n (assert (sub1 n) exact-nonnegative-integer?)))))]))))

(: first-answer
   (∀ (a) (→ (→ a) (Values (U False (List a)) (Generatorof (List a))))))
(define (first-answer thk)
  (let loop ([gen : (Generatorof (List a)) (λ () ((inst amb-generate* a) thk))])
    (define-values (v next) (gen))
    (cond
      [(exact-positive-integer? v)
       (for ([_ (in-range v)]) (amb-pause))
       (loop next)]
      [else (values v next)])))
