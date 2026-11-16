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

The search strategy is the
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

@section[#:tag "performance"]{Performance}

The examples of the book that the tests of each edition cover were run
with ambKanren and with the stream-based implementation of the same
edition in miniKanren-typed.  The table gives the median of three runs
of the compiled test modules (Racket 9.3 CS), and the memory that each
run allocated in all.

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         #:column-properties '(left right right right right)
(list (list @bold{edition} @bold{miniKanren-typed} @bold{ambKanren} @bold{ratio} @bold{allocated (miniKanren-typed / ambKanren)})
      (list "first (228 examples)" "15.7 s" "16.9 s" "1.08" "2.8 GB / 29.5 GB")
      (list "second (285 examples)" "13.6 s" "15.1 s" "1.11" "6.1 GB / 19.8 GB"))]

The times are close, but ambKanren allocates much more.  Each effect of
a search (a choice, a pause, the resumption of an alternative)
captures or reinstates a continuation, at a cost of about a kilobyte,
where a stream allocates a closure or a pair.  The first edition
allocates the most, since each of its conjunctions is a local search.
The measurements include the checks of the tests themselves, and vary
with the machine and the version of Racket.
