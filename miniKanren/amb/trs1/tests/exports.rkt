#lang racket/base

(require rackunit racket/list)

(define (phase-0-exports mod)
  (dynamic-require mod #f)
  (define-values (variables syntaxes) (module->exports mod))
  (remove-duplicates
   (for*/list ([phase (in-list (append variables syntaxes))]
               #:when (zero? (car phase))
               [entry (in-list (cdr phase))])
     (car entry))))

(define exports (phase-0-exports 'typed/miniKanren/amb/trs1))
(define reference (phase-0-exports 'typed/miniKanren/trs1))

;; Only representation helpers remain specific to ambKanren's TRS1.
(check-equal?
 (sort (filter (λ (name) (not (memq name reference))) exports) symbol<?)
 '(list->amb sequence->amb))

(for ([name '(Literal Data data?)])
  (check-not-false (memq name exports)))

;; Check both implementation entry points as well as the public wrapper.
(for ([mod '(typed/miniKanren/amb/trs1/main
             typed/miniKanren/amb/trs1/base)])
  (define names (phase-0-exports mod))
  (for ([name '(bind bindi conj conj+ conji conji+ disj disj+ disji disji+
                Zzz apply-goal apply-s if-aux s-empty? s-has-key? var=?
                project run∞ ≡ ∃ ∧ ∨ ∧i ∨i ⊤ ⊥)])
    (check-false (memq name names) (format "~a exports ~a" mod name))))
