#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb in-amb/do)
                     ambKanren/trs1))

@title{ambKanren: miniKanren Based on amb}
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

ambKanren is a miniKanren in which backtracking is not implemented by a
stream of answers but delegated to the ambiguous operator @racket[amb]
of the @racketmodname[typed/amb] library.  A goal is an ordinary
function from substitutions to substitutions; when a goal fails it
calls @racket[(amb)], and when it has several answers it chooses one of
them with @racket[amb].  The search over all choices is performed by
@racket[in-amb/do], which @racket[run] uses to collect answers.

The package provides one module for each language it implements:

@itemlist[

@item{@racketmodname[ambKanren/demo]: a minimal version in the style
of microKanren, intended for reading.}

@item{@racketmodname[ambKanren/trs1]: the language of the first
edition of @italic{The Reasoned Schemer} (Friedman, Byrd, and
Kiselyov, MIT Press, 2005).}

@item{@racketmodname[ambKanren/trs2]: the language of the second
edition (Friedman, Byrd, Kiselyov, and Hemann, MIT Press, 2018).}

]

@section[#:tag "correspondence"]{Correspondence with Stream-Based miniKanren}

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
            @racket[(→ Substitution (Streamof Substitution))]
            @racket[(→ Substitution Substitution)])
      (list @racket[succeed] @racket[(λ (s) (unit s))] @racket[(λ (s) s)])
      (list @racket[fail] @racket[(λ (s) (mzero))] @racket[(λ (s) (amb))])
      (list @racket[conj] @racket[(λ (s) (bind (g1 s) g2))] @racket[(λ (s) (g2 (g1 s)))])
      (list @racket[disj] @racket[(λ (s) (mplus (g1 s) (g2 s)))] @racket[(λ (s) ((amb g1 g2) s))])
      (list @racket[run] @elem{take answers from the stream @racket[(g empty-s)]} @elem{take answers from @racket[(in-amb/do (g empty-s))]}))]

So @racket[succeed] is the identity function, conjunction is plain
function composition, and disjunction is a choice made by
@racket[amb].  The monadic structure has not gone away: it now lives in
the continuation captured by @racket[amb], and @racket[in-amb/do] is
what turns that control effect back into a sequence of answers.

@table-of-contents[]

@include-section["demo.scrbl"]
@include-section["trs1.scrbl"]
@include-section["trs2.scrbl"]
