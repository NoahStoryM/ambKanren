#lang typed/racket/base

(define-namespace-anchor main)
(define main-namespace (namespace-anchor->empty-namespace main))
(parameterize ([current-namespace main-namespace])
  (namespace-require 'typed/racket/base)
  (namespace-require "../../milli.rkt")
  (namespace-require "../../main.rkt")
  (namespace-require '(all-except typed/rackunit fail))
  (load "ch1.rktl")
  (load "ch2.rktl")
  (load "ch3.rktl"))
