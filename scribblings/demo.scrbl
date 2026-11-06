#lang scribble/manual

@(require (for-label typed/racket/base
                     ambKanren/trs1)
          scribble/example)

@(define demo-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require ambKanren/demo)))

@title[#:tag "demo"]{A Minimal Version}
@defmodule[ambKanren/demo #:packages ("ambKanren")]

This module is a self-contained, minimal miniKanren based on
@racket[amb], close in size and structure to microKanren.  It is
intended for reading rather than for use: every definition is a few
lines long, and the whole correspondence of @secref["correspondence"]
can be seen at once.

The core (@racket[walk], @racket[unify], binary @racket[disj] and
@racket[conj], @racket[Zzz], @racket[conj+] and @racket[disj+], and
reification) follows microKanren (Hemann and Friedman, @italic{μKanren:
A Minimal Functional Core for Relational Programming}, 2013), with the
stream operations replaced by @racket[amb].  Substitutions are
association lists, and the user-level forms are @racket[run],
@racket[run*], @racket[fresh], @racket[all], and @racket[conde].  The
differences from microKanren are:

@itemlist[

@item{There is no stream type and no fresh-variable counter.
Variables are created directly by @racket[fresh], as in @italic{The
Reasoned Schemer}, rather than by microKanren's @tt{call/fresh} on a
state.}

@item{@racket[ext-s] performs the occurs check; failure is signalled by
@racket[(amb)] rather than by returning @racket[#f].}

]

The other modules of this package build on the same core and export
many of the same names (@racket[Term], @racket[Substitution],
@racket[Goal], @racket[var], @racket[walk], @racket[unify],
@racket[==], @racket[succeed], @racket[fail], @racket[reify], and so
on), so this module should not be required together with them.

@examples[#:eval demo-eval
(run* (q) (fresh (a b) (conde [(== a 1)] [(== a 2)]) (== b a) (== q (cons a b))))
]
