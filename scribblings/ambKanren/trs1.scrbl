#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb amb-fail in-amb/do amb-generate* amb-local* current-amb-merger)
                     (only-in amb/merge amb-append amb-alternate)
                     typed/miniKanren/amb/trs1)
          scribble/example
          "utils.rkt"
          "minikanren.rkt")

@(define ak-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/miniKanren/amb/trs1 typed/amb)))
@(define-syntax-rule (ak-examples body ...)
   (examples #:eval ak-eval body ...))

@title[#:tag "trs1"]{First Edition}
@defmodule[typed/miniKanren/amb/trs1 #:packages ("ambKanren")]

This module implements the language of the first edition of
@italic{The Reasoned Schemer} on top of @racket[amb].

@section[#:tag "trs1-basis"]{Implementation Basis}

The core follows @secref["microKanren"]: a goal returns one
substitution and chooses among its answers with @racket[amb].  Unlike
microKanren, there is no state and no variable counter (@racket[fresh]
creates variables directly).  Substitutions are association lists,
as in @tt{miniKanren-typed}.

The user-level language is that of the @emph{first} edition of @italic{The Reasoned Schemer}
(Friedman, Byrd, and Kiselyov, MIT Press, 2005): @racket[run] with a
single query variable, @racket[conde] with @racket[else], the
interleaving forms @racket[condi] and @racket[alli], and the
committed-choice forms @racket[conda] and @racket[condu].  ``Chapter
@italic{n}'' below refers to that edition.

The differences from the book are:

@itemlist[

@item{Goals use @racket[amb] in place of streams.  Unification through
@racket[==] uses the occurs check, as in @tt{miniKanren-typed}, and
fails with @racket[(amb-fail)].}

@item{@racket[conde] is depth-first, as in the first edition, and
uses @racket[amb-append].  The interleaving forms @racket[condi] and
@racket[alli] follow @mk1[mplusi] and @mk1[bindi] of the first edition
(see @secref["trs1-interleaving"]).  With the default shuffler, the
book examples covered by the tests give the same answers in the same
order.}

]

@section[#:tag "trs1-types"]{Types}

@deftype[Literal]{
Booleans, numbers, characters, byte strings, strings, keywords,
@racket['()], and symbols.
}

@deftype[Data]{
Defined as @racket[(∪ Literal (Pair Data Data))]: data without logic variables.
}

@defproc[(data? [v Any]) Boolean]{
Recognizes @racket[Data], with a Typed Racket occurrence-typing filter.
}

@deftype[Term]{
The type of terms: booleans, numbers, characters, byte strings,
strings, keywords, symbols, @racket['()], logic variables
(@racket[Var]), and pairs of terms.
}

@deftogether[(@deftype[Var]
              @defstruct*[var ([name Symbol]) #:transparent])]{
Logic variables.  Variables are compared by identity
with @racket[eq?]; the name is only for printing.
}

@deftype[Substitution]{
Defined as @racket[(Listof (Pair Var Term))], searched with
@racket[assq] (variable identity).  As in other miniKanren implementations, the
substitution is triangular: a variable may be associated with a term
containing other associated variables.
}

@deftype[Goal]{
Defined as @racket[(→ Substitution Substitution)].  Applying a goal to
a substitution returns one extension of it under which the goal holds,
chosen with @racket[amb]; the other extensions are explored when the
search backtracks.  A goal with no answer calls @racket[(amb-fail)].
Goals must therefore be applied within the dynamic extent of an
@racket[amb] search, such as @racket[in-amb/do].

@ak-examples[
(define x (var 'x))
(for/list : (Listof Term)
          ([s : Substitution
              (in-amb/do ((conde [(== x 1)] [(== x 2)]) empty-s))])
  (walk x s))
]
}

@section[#:tag "trs1-interface"]{Running Programs}

@deftogether[(@defform[(run n (x) goal ...)
                       #:contracts ([n (Option Natural)])]
              @defform[(run* (x) goal ...)])]{
Introduces a fresh variable @racket[x], applies the conjunction of the
@racket[goal]s to the empty substitution inside @racket[amb-generate*],
and returns a list of at most @racket[n] reified values of
@racket[x].  When @racket[n] is @racket[#f], all the values are
returned, and the computation does not terminate if there are
infinitely many.  The limit is checked before each answer is
requested, so @racket[run] never searches for an answer that it will
not return.  (Chapter 1.)

@ak-examples[
(run* (q) (fresh (a b) (== q (list a b)) (== a 'left)))
]
}

@section[#:tag "trs1-goals"]{Goals}

@deftogether[(@defthing[succeed Goal]
              @defthing[fail Goal])]{
@racket[succeed] is the identity function; @racket[fail] calls
@racket[(amb-fail)].  (Chapter 1.)
}

@defproc[(== [u Term] [v Term]) Goal]{
The goal that extends the substitution by @racket[unify]ing @racket[u]
with @racket[v], and fails if they do not unify.  (Chapter 1.)

@ak-examples[
(run* (q) (fresh (x) (== q (cons x 2)) (== x 1)))
(run* (q) (== q (list q)))
]
}

@defform[(fresh (x ...) goal ...)]{
Binds each @racket[x] to a new variable and behaves as
@racket[(all goal ...)].  Each application creates the variables
before running the conjunction.  The form with no variables also
delays the conjunction until application.  (Chapter 1.)
}

@defform[(λG (s) body ...+)]{
A function annotated with the type @racket[Goal].  For example,
@racket[(λG (s) ((== x 1) s))] is a goal.
}

@deftogether[(@defform[(all goal ...)]
              @defform[(alli goal ...)])]{
Conjunction.  With no goals, both forms are @racket[succeed].
A single goal is returned unchanged.  With multiple goals, the first
goal expression is evaluated when the conjunction is built; the rest
is built when the resulting goal is applied, as in the reference implementation.
@racket[all] explores answers depth-first with @racket[amb-append];
@racket[alli] interleaves them with @racket[amb-alternate], binding
answers through a local search.  (Chapter 6.)
}

@deftogether[(@defform[#:literals (else)
                       (conde clause ...)
                       #:grammar ([clause [goal ...+]
                                          [else goal ...]])]
              @defform[(condi clause ...)])]{
Disjunction of clauses, each of which is a conjunction.  An
@racket[else] clause, if present, comes last.  @racket[conde] tries
the clauses depth-first; @racket[condi] interleaves their answers.
(@racket[conde], Chapter 1; @racket[condi], Chapter 6.)

@ak-examples[
(: repeato (→ Term Term Goal))
(define (repeato x l)
  (conde
    [(== l '())]
    [(fresh (d) (== l (cons x d)) (repeato x d))]))
(run 6 (q) (conde [(repeato 'a q)] [(repeato 'b q)]))
(run 6 (q) (condi [(repeato 'a q)] [(repeato 'b q)]))
]
}

@deftogether[(@defform[(conda clause ...)]
              @defform[(condu clause ...)])]{
Committed choice.  The first goal of each non-final clause is a
test.  Once a test succeeds, that clause is the only one used.
@racket[conda] continues with every answer of the test and
@racket[condu] continues with its first answer.  If no earlier test
succeeds, the final clause runs as an ordinary @racket[all],
including when it is the only clause.  (Chapter 10.)

@ak-examples[
(run* (q) (conda [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
(run* (q) (condu [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
(run* (q) (condu [(conde [(== q 1)] [(== q 2)])]))
]
}

@deftogether[(@defform[(ife g0 g1 g2)]
              @defform[(ifi g0 g1 g2)]
              @defform[(ifa g0 g1 g2)]
              @defform[(ifu g0 g1 g2)]
              @defform[(cond-aux ifer clause ...)]
              @defform[(all-aux bnd goal ...)])]{
The building blocks of the forms above, with the structure of the
first edition's implementation.  @racket[cond-aux] turns each
non-final clause into @racket[(ifer test (all goal ...) rest)] and
the final clause into @racket[all].  @racket[all-aux] follows
@tt{miniKanren-typed}: its empty case is @racket[succeed], its
single-goal case is that goal unchanged, and its recursive case
applies @racket[(bnd (g^ s) (all-aux bnd goal ...))].
The private binding forms used by @racket[all] and @racket[alli]
capture the first argument in a local @racket[amb] search; they are
not exported stream-binding functions.  @racket[ifa] and
@racket[ifu] search for the first answer of the test with
@racket[amb-generate*], passing on every pause, so that they know
whether it has an answer before they commit.
}

@subsection[#:tag "trs1-interleaving"]{Interleaving}

@racket[condi] chooses clauses with @racket[amb-alternate], which
passes the turn at every answer.  @racket[alli] binds the answers of
each non-final goal through a local search (@racket[amb-local])
under the same merger and continues from each answer.  These forms
take the place of the stream-based @mk1[mplusi] and @mk1[bindi].

In the example below, @racket[counto] has an answer for every length,
and the answers with @racket['R] are still reached:

@ak-examples[
(: counto (→ Term Goal))
(define (counto n)
  (conde
    [(== n '())]
    [(fresh (m) (== n (cons 'I m)) (counto m))]))
(run 6 (q)
  (fresh (n side)
    (alli (counto n) (conde [(== side 'L)] [(== side 'R)]))
    (== q (cons side n))))
]

@section[#:tag "trs1-substitutions"]{Substitutions and Unification}

@deftogether[(@defthing[empty-s Substitution]
              @defproc[(size-s [s Substitution]) Index]
              @defproc[(ext-s [x Var] [v Term] [s Substitution]) Substitution])]{
Operations on substitutions.  @racket[ext-s] associates @racket[x]
with @racket[v].  Ground data (recognized by @racket[data?]) skips the
occurs check; otherwise the operation fails with @racket[(amb-fail)]
when @racket[x] occurs in @racket[v].
}

@deftogether[(@defproc[(walk [v Term] [s Substitution]) Term]
              @defproc[(walk* [v Term] [s Substitution]) Term]
              @defproc[(occurs? [x Var] [v Term] [s Substitution]) Boolean]
              @defproc[(unify [u Term] [v Term] [s Substitution]) Substitution])]{
@racket[walk] follows the associations of a variable until it reaches
a term that is not an associated variable; @racket[walk*] does so
throughout a term.  @racket[unify] returns an extension of @racket[s]
that makes @racket[u] and @racket[v] equal, and calls @racket[(amb-fail)]
if there is none.  (Chapter 9.)
}

@deftogether[(@defproc[(reify-name [n Index]) Symbol]
              @defproc[(reify-s [v Term] [s Substitution]) Substitution]
              @defproc[(reify [v Term]) Term])]{
Reification: unassociated variables are replaced by @racket['_.0],
@racket['_.1], and so on, in order of first appearance.  (Chapter 9.)
}

@deftogether[(@defproc[(sequence->amb [s (Sequenceof a)]) a]
              @defproc[(list->amb [l (Listof a)]) a])]{
Choose an element with @racket[amb].  These operations belong to the
@racket[amb] representation and have no stream counterpart in this
module's interface.
}
