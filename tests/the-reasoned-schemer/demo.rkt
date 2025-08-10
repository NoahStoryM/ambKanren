#lang typed/racket/base

(define-namespace-anchor demo)
(define demo-namespace (namespace-anchor->empty-namespace demo))
(parameterize ([current-namespace demo-namespace])
  (namespace-require 'typed/racket/base)
  (namespace-require "../../demo.rkt")
  (namespace-require '(all-except typed/rackunit fail))
  (load "ch1.rktl"))
