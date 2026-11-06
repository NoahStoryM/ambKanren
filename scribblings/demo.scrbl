#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb in-amb/do)
                     ambKanren/trs1)
          scribble/example
          "minikanren.rkt")

@(define demo-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require ambKanren/demo)))

@title[#:tag "demo"]{From Streams to amb}

In a stream-based miniKanren, a goal has type
@racket[(→ Substitution (Streamof Substitution))]: it returns every
answer at once, lazily.  The stream type is a monad, and the
connectives are its operations: @racket[succeed] is @emph{unit},
@racket[fail] is @emph{mzero}, disjunction is @emph{mplus}, and
conjunction is @emph{bind}, that is, composition in the Kleisli
category of the stream monad.

With @racket[amb], the nondeterminism is an effect of evaluation
rather than a value, so it disappears from the type of goals:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
(list (list @bold{} @bold{stream monad} @bold{ambKanren})
      (list "goal"
            @mk1[(→ Substitution (Streamof Substitution))]
            @racket[(→ Substitution Substitution)])
      (list @racket[succeed] @mk1[(λ (s) (unit s))] @racket[(λ (s) s)])
      (list @racket[fail] @mk1[(λ (s) (mzero))] @racket[(λ (s) (amb))])
      (list @racket[conj] @mk1[(λ (s) (bind (g1 s) g2))] @racket[(λ (s) (g2 (g1 s)))])
      (list @racket[disj] @mk1[(λ (s) (mplus (g1 s) (g2 s)))] @racket[(λ (s) ((amb g1 g2) s))])
      (list @racket[run] @elem{take answers from the stream @mk1[(g empty-s)]} @elem{take answers from @racket[(in-amb/do (g empty-s))]}))]

So @racket[succeed] is the identity function, conjunction is plain
function composition, and disjunction is a choice made by
@racket[amb].  The monadic structure has not gone away: it now lives in
the continuation captured by @racket[amb], and @racket[in-amb/do] is
what turns that control effect back into a sequence of answers.

@section[#:tag "demo-module"]{A Minimal Version}

@defmodule[ambKanren/demo #:packages ("ambKanren")]

This module is a self-contained, minimal miniKanren based on
@racket[amb], close in size and structure to microKanren.  It is
intended for reading rather than for use: every definition is a few
lines long, and the whole correspondence above can be seen at once.

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
