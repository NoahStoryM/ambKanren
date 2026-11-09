#lang racket/base
;; Counterexample: with a SEPARATE pause prompt tag, a pause raised
;; inside an in-amb/do that did not install that prompt escapes the
;; inner search and silently loses answers.  Expected (1) and (1 2).
;; Run: racket pause-escape.rkt
(require amb "kernel.rkt"
         "schemes.rkt")
(use-scheme! pause/disj)
(defrel (oneo x) (== x 1))
(define q (var 'q))
;; a direct-style goal that runs its own in-amb/do WITHOUT a pause prompt
(define ((raw-once g) s)
  (define-values (more? get) (sequence-generate (in-amb/do (g s))))
  (if (more?) (get) (amb)))
(printf "raw in-amb/do around a pausing goal: ~s\n"
        (with-handlers ([exn? exn-message])
          (run* q (raw-once (oneo q)) (== q 1))))
(printf "same, wrapped by a local disj: ~s\n"
        (with-handlers ([exn? exn-message])
          (run* q (disj (raw-once (oneo q)) (== q 2)))))
