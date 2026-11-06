#lang info

(define license 'MIT)
(define collection "typed")
(define version "2.0")

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

(define scribblings
  '(("scribblings/ambKanren/ambKanren.scrbl" (multi-page) (library) "ambKanren")))

(define clean
  '("compiled"
    "miniKanren/amb/compiled"
    "miniKanren/amb/trs1/compiled"
    "miniKanren/amb/trs1/private/compiled"
    "miniKanren/amb/trs2/compiled"
    "miniKanren/amb/demo/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
