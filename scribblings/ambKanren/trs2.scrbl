#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb in-amb/do amb-generate* amb-local amb-pause current-amb-merger)
                     (only-in amb/merge amb-interleave amb-alternate amb-level amb-append)
                     typed/miniKanren/amb/trs2)
          scribble/example
          "utils.rkt"
          "minikanren.rkt")

@(define ak-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/miniKanren/amb/trs2 typed/amb typed/amb/merge)
                   '(current-amb-merger amb-interleave)))
@(define-syntax-rule (ak-examples body ...)
   (examples #:eval ak-eval body ...))

@title[#:tag "trs2"]{Second Edition}
@defmodule[typed/miniKanren/amb/trs2 #:packages ("ambKanren")]

This module implements the language of the second edition of
@italic{The Reasoned Schemer} (Friedman, Byrd, Kiselyov, and Hemann,
MIT Press, 2018) on top of @racket[amb], without streams.

@section[#:tag "trs2-basis"]{Implementation Basis}

The interface (@racket[run], @racket[defrel], @racket[fresh],
@racket[conde], @racket[conda], @racket[condu], @racket[project]) and
the definitions of @racket[walk], @racket[unify], @racket[ifte],
@racket[once], and reification follow the official code of the second
edition.  What changes is the representation of goals:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
(list (list @bold{} @bold{second edition} @bold{typed/miniKanren/amb/trs2})
      (list "goal" @mk2[(→ Substitution (Streamof Substitution))] @racket[(→ Substitution Substitution)])
      (list @racket[disj2] @mk2[append-inf] @elem{a choice made by @racket[amb]})
      (list @racket[conj2] @mk2[append-map-inf] @elem{a local search: @racket[(g2 (amb-local (g1 s)))]})
      (list "suspension" @elem{an immature stream, @mk2[(λ () ...)]} @elem{@racket[suspend]: one logical pause}))]

The second edition suspends every relation call.  Here
@racket[defrel] calls @racket[amb-pause]; with
@racket[current-amb-merger] set to @racket[amb-interleave], that pause
lets the other side of the binary choice run.  The examples below
explicitly select this merger:

@ak-examples[
(defrel (nevero) (nevero))
(run 1 q (conde [(nevero)] [succeed]))
]

@subsection[#:tag "trs2-order"]{Order of Answers}

The strategy is @racket[current-amb-merger], rather than the global
frontier of amb v2.  With @racket[amb-interleave], all 285 book
examples agree exactly, in the order of the book.

This depends on @racket[conj2].  Like @mk2[append-map-inf], it searches
@racket[g1] locally and continues with @racket[g2] from each answer, so
the continuations of @racket[g2] are interleaved in the order of the
answers of @racket[g1], with the pauses of @racket[g1] in between.
Plain function composition, @racket[(g2 (g1 s))], gives the same
answers, but runs @racket[g2] inside the choices of @racket[g1], so they
are interleaved in the shape of those choices.  With composition,
@racket[(conj2 (disj2 a b) g)] is @racket[(disj2 (conj2 a g) (conj2 b g))],
order included; the book's bind keeps the answers of
@racket[(disj2 a b)] together.  With composition, two examples
(Chapter 8 and Chapter 9) return their answers in another order.  The local search costs one more search per conjunction.

@racket[amb-alternate] changes order at answers too; @racket[amb-level]
uses synchronized pause rounds.  The migration report records each
differing example and the tested conditions.  These mergers need not
give the same bounded prefix.

@section[#:tag "trs2-types"]{Types}

@deftype[Term]{
The type of terms: booleans, numbers, characters, byte strings,
strings, keywords, symbols, @racket['()], logic variables
(@racket[Var]), and pairs of terms.
}

@deftogether[(@deftype[Var]
              @defstruct*[var ([name Symbol]) #:transparent])]{
Logic variables, compared by identity.
}

@deftype[Substitution]{
Defined as @racket[(Immutable-HashTable Var Term)], with
@racket[eq?]-based keys.
}

@deftype[Goal]{
Defined as @racket[(→ Substitution Substitution)].  A goal returns one
extension of the substitution, chosen with @racket[amb], and calls
@racket[(amb)] when it has none.
}

@section[#:tag "trs2-interface"]{Running Programs}

@defform*[((run n q goal ...)
           (run n (x ...+) goal ...))
          #:contracts ([n (Option Natural)])]{
Runs the conjunction of the @racket[goal]s and returns at most
@racket[n] reified answers, or all of them when @racket[n] is
@racket[#f].  In the second form, each answer is the list of the values
of the @racket[x]s.  (Chapter 1.)

@ak-examples[
(run* (x y) (conde [(== x 'a)] [(== x 'b)]) (== y x))
]
}

@defform[(run* q goal ...)]{
Like @racket[run] with @racket[n] equal to @racket[#f].
}

@section[#:tag "trs2-goals"]{Goals and Relations}

@deftogether[(@defthing[succeed Goal]
              @defthing[fail Goal]
              @defproc[(== [u Term] [v Term]) Goal])]{
Success, failure, and unification with the occurs check.  (Chapter 1.)
}

@defform[#:literals (:)
         (defrel (name arg ...) goal ...)
         #:grammar ([arg x [x : type]])]{
Defines @racket[name] as a relation whose result is a @racket[Goal]
that calls @racket[suspend] before running the conjunction of the
@racket[goal]s.  A plain argument @racket[x] has type @racket[Term];
an argument written as @racket[[x : type]] has the given type.
(Chapter 1.)
}

@defproc[(suspend) Void]{
One logical pause, implemented by @racket[amb-pause].  The merger at
its enclosing choice determines whether another alternative gets a
turn.  Use @racket[amb-interleave] for pause-based binary scheduling,
@racket[amb-alternate] to switch at answers too, or @racket[amb-level]
for rounds.  @racket[amb-append] keeps the turn at a pause.
}

@deftogether[(@defform[(fresh (x ...) goal ...)]
              @defform[(conde [goal ...+] ...)]
              @defform[(conda [goal ...+] ...+)]
              @defform[(condu [goal ...+] ...+)]
              @defform[(project (x ...) goal ...)])]{
As in the second edition.  @racket[conde] is the disjunction of the
conjunctions of its lines; @racket[conda] and @racket[condu] commit to
the first line whose first goal succeeds, keeping all of its answers or
only the first.  (@racket[fresh] and @racket[conde], Chapter 1;
@racket[conda] and @racket[condu], Chapter 9; @racket[project],
Chapter 10.)
}

@deftogether[(@defform[(disj goal ...)]
              @defform[(conj goal ...)]
              @defproc[(disj2 [g1 Goal] [g2 Goal]) Goal]
              @defproc[(conj2 [g1 Goal] [g2 Goal]) Goal])]{
Disjunction and conjunction.  @racket[disj2] applies the goal chosen by
@racket[(amb g1 g2)], and @racket[conj2] applies @racket[g2] to each
answer of a local search of @racket[g1], as
@racket[(g2 (amb-local (g1 s)))]; see @secref["trs2-order"].
}

@deftogether[(@defproc[(ifte [g1 Goal] [g2 Goal] [g3 Goal]) Goal]
              @defproc[(once [g Goal]) Goal])]{
The functions behind @racket[conda] and @racket[condu].  They search
for the first answer with @racket[amb-generate*], forwarding every
logical pause with @racket[amb-pause].  This keeps an enclosing search
schedulable while the test runs.  A sequence observer that skips pauses
would hide them from the outer merger.
}

@deftogether[(@defproc[(call/fresh [name Symbol] [f (→ Var Goal)]) Goal]
              @defproc[(run-goal [n (Option Natural)] [g Goal])
                       (Listof Substitution)])]{
The functions behind @racket[fresh] and @racket[run].
}

@section[#:tag "trs2-substitutions"]{Substitutions and Unification}

@deftogether[(@defthing[empty-s Substitution]
              @defproc[(walk [v Term] [s Substitution]) Term]
              @defproc[(walk* [v Term] [s Substitution]) Term]
              @defproc[(occurs? [x Var] [v Term] [s Substitution]) Boolean]
              @defproc[(ext-s [x Var] [v Term] [s Substitution]) Substitution]
              @defproc[(unify [u Term] [v Term] [s Substitution]) Substitution])]{
As in the second edition, except that @racket[ext-s] and @racket[unify]
call @racket[(amb)] instead of returning @racket[#f].
}

@deftogether[(@defproc[(reify-name [n Index]) Symbol]
              @defproc[(reify-s [v Term] [r Substitution]) Substitution]
              @defproc[(reify [v Term]) (→ Substitution Term)])]{
Reification, with the names @racket['_0], @racket['_1], and so on.
}
