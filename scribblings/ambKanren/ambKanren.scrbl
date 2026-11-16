#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb amb-fail amb-generate*)
                     typed/miniKanren/amb/trs1))

@title{ambKanren: miniKanren Based on amb}
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

ambKanren is a miniKanren whose search is not a stream of answers but
the ambiguous operator @racket[amb] of the @racketmodname[typed/amb]
library.  A goal is an ordinary function from substitutions to
substitutions (from states to states, in microKanren).  A goal with
several answers chooses one of them with @racket[amb]; a goal with none
calls @racket[(amb-fail)].  @racket[run] collects the answers with
@racket[amb-generate*].

This version requires typed-amb 3.0.  The search strategy is the
merger of @racket[amb], the value of
@racketidfont{current-amb-merger}.  The default merger,
@racketidfont{amb-append}, is depth-first.  microKanren and the second
edition pause at every relation call, as the books suspend there, so
their recursive programs need @racketidfont{amb-interleave}, the
interleaving of the books.  The first edition does not pause: its
interleaving forms choose their own merger.

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
