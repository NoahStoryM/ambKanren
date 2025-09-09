#lang info

(define license 'MIT)
(define collection "ambKanren")
(define version "0.0")

(define pkg-desc "miniKanren based on amb")

(define deps '("base" "typed-racket-lib" "typed-amb" "typed-data-queue"))
(define build-deps '("rackunit-typed"))
#;
(define scribblings '(("scribblings/ambKanren.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
