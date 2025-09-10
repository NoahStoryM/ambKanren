#lang racket/base

(require data/queue)
(provide (all-defined-out))

(define (rotate-queue! q)
  (when (non-empty-queue? q)
    (enqueue! q (dequeue! q))))
