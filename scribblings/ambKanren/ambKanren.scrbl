#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb amb-fail in-amb/do)
                     typed/miniKanren/amb/trs1))

@title{ambKanren: miniKanren Based on amb}
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

ambKanren is a miniKanren in which backtracking is not implemented by a
stream of answers but delegated to the ambiguous operator @racket[amb]
of the @racketmodname[typed/amb] library.  A goal is an ordinary
function from substitutions to substitutions (from states to states, in
microKanren); when a goal fails it
calls @racket[(amb)] or @racket[(amb-fail)], and when it has several
answers it chooses one of them with @racket[amb].  The search over all
choices is performed by @racket[in-amb/do], which @racket[run] uses to
collect answers.

The aim is to use the control effects of the host language in place of
a hand-written monad of streams, and to keep the laws of the search
that this brings.  Where the books' streams do not obey those laws, as
the interleaving of the second edition and of microKanren does not,
ambKanren keeps the laws and gives up the books' order of answers: it
finds the same answers, fairly, in an order that can differ.  The
miniKanren-typed package implements the books' streams as they are.

The package provides one module for each language it implements:

@itemlist[

@item{@racketmodname[typed/microKanren/amb]: microKanren (Hemann and
Friedman, 2013), the minimal functional core.}

@item{@racketmodname[typed/miniKanren/amb/trs1]: the language of the first
edition of @italic{The Reasoned Schemer} (Friedman, Byrd, and
Kiselyov, MIT Press, 2005).}

@item{@racketmodname[typed/miniKanren/amb/trs2]: the language of the second
edition (Friedman, Byrd, Kiselyov, and Hemann, MIT Press, 2018).}

]

@table-of-contents[]

@include-section["microKanren.scrbl"]
@include-section["trs1.scrbl"]
@include-section["trs2.scrbl"]
