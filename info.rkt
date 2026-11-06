#lang info

(define license 'MIT)
(define collection "ambKanren")
(define version "0.0")

(define pkg-desc "miniKanren based on amb")

(define deps
  '("base"
    "typed-racket-lib"
    "typed-data-queue"
    ["typed-amb" #:version "2.0"]))
(define build-deps
  '("at-exp-lib"
    "rackunit-typed"
    "miniKanren-typed"
    "scribble-lib"
    "racket-doc"
    "typed-racket-doc"
    "amb"))

(define scribblings '(("scribblings/ambKanren.scrbl" (multi-page))))

(define clean '("compiled" "trs1/compiled" "trs1/private/compiled" "demo/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
