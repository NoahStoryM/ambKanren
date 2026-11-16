#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb amb-fail amb-generate*)
                     typed/miniKanren/amb/trs1))

@title{ambKanren: miniKanren Based on amb}
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

ambKanren compares @racket[amb] with the books' streams and tests the
resulting behavior.  The three implementations documented below follow
@tt{miniKanren-typed},
keeping the non-search code the same while replacing streams with
@racket[amb].

Its search uses the ambiguous operator @racket[amb] of the @racketmodname[typed/amb]
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

The compiled book tests were run with ambKanren and the stream-based
implementation in miniKanren-typed, using the same association-list
substitutions, ground-data shortcut and non-search definitions.
The first edition includes chapters 1--10 (254 checks); the second
includes chapters 1--9 and arithmetic (285 checks).  Both use the same
source test files and exact assertions, with no answer-order exceptions.

Each implementation was run three times in a fresh process, strictly
alternating streams and amb within each edition.  The CPU table shows
the median (minimum--maximum) in seconds; the allocation table shows
rounded medians of cumulative allocated GB (10^9 bytes, not peak
memory).  Exact allocation values and ranges are in the report linked
below.  The ratio is amb's CPU median divided by the stream
implementation's median.

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         #:column-properties '(left right right right)
(list (list @bold{edition} @bold{streams: CPU s} @bold{amb: CPU s} @bold{ratio})
      (list "TRS1" "27.95 (26.98–29.29)" "54.01 (53.31–62.94)" "1.93")
      (list "TRS2" "22.12 (20.43–22.81)" "31.44 (29.22–33.95)" "1.42"))]

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         #:column-properties '(left right right)
(list (list @bold{edition} @bold{streams: allocated GB} @bold{amb: allocated GB})
      (list "TRS1" "2.7" "26.8")
      (list "TRS2" "6.1" "19.4"))]

These measurements used Racket 9.3 CS on Linux x86-64, an Intel Xeon
Platinum 8573C container with nine visible CPUs (affinity 0--8).
Modules were compiled before measurement; process startup and
compilation are excluded, while loading the compiled test modules and
their checks is included.  A full garbage collection precedes each
timed region.  CPU frequency and host load were not controlled, so
results vary with the machine and the version of Racket.

amb is slower and allocates substantially more in both
editions.  Choices, pauses and local searches use continuation and
generator machinery in place of stream pairs and suspensions; in the
first edition, each nonfinal conjunction introduces a local search.
The larger allocation is consistent with that mechanism, but this
whole-module measurement does not isolate how much CPU time is spent
capturing or restoring continuations.

Raw samples, real and GC times, source hashes, reproduction commands
and the complete source comparison are in
@hyperlink["https://github.com/NoahStoryM/ambKanren/tree/archive/v3-research/research/v3-results/alignment-20261116"]{the alignment measurement report}.
