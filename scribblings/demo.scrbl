#lang scribble/manual

@(require (for-label typed/racket/base
                     ambKanren/trs1)
          scribble/example)

@(define demo-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require ambKanren/demo)))

@title[#:tag "demo"]{A Minimal Version}
@defmodule[ambKanren/demo #:packages ("ambKanren")]

This module is a self-contained, minimal version of the core, close in
size and structure to microKanren: substitutions are association lists,
@racket[disj] and @racket[conj] are binary, and the user-level forms
are @racket[run], @racket[run*], @racket[fresh], @racket[all], and
@racket[conde].  It is intended for reading rather than for use: every
definition is a few lines long, and the whole correspondence of
@secref["correspondence"] can be seen at once.

The module exports the same names as the corresponding parts of
@racketmodname[ambKanren/trs1] (@racket[Term], @racket[Substitution],
@racket[Goal], @racket[var], @racket[walk], @racket[unify],
@racket[==], @racket[disj], @racket[conj], @racket[succeed],
@racket[fail], @racket[Zzz], @racket[disj+], @racket[conj+],
@racket[reify], and so on), so the two modules should not be required
together.

@examples[#:eval demo-eval
(run* (q) (fresh (a b) (conde [(== a 1)] [(== a 2)]) (== b a) (== q (cons a b))))
]
