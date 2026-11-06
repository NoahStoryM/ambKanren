#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb in-amb/do current-amb-depth-first?)
                     typed/miniKanren/amb/trs2)
          scribble/example
          "utils.rkt"
          "minikanren.rkt")

@(define ak-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/miniKanren/amb/trs2)))
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
      (list @racket[conj2] @mk2[append-map-inf] "function composition")
      (list "suspension" @elem{an immature stream, @mk2[(λ () ...)]} @elem{@racket[suspend]: a choice point sent to the back of the frontier}))]

The second edition can switch away from a branch that never produces
an answer, because @racket[defrel] suspends every call to a relation
and @mk2[append-inf] swaps its two streams at each suspension.  Here,
@racket[defrel] calls @racket[suspend], which lets the computations
already waiting in @racket[amb]'s frontier run first:

@ak-examples[
(defrel (nevero) (nevero))
(run 1 q (conde [(nevero)] [succeed]))
]

@subsection[#:tag "trs2-order"]{Order of Answers}

The two implementations produce the same answers but schedule
suspended computations differently.  @mk2[append-inf] swaps a suspended
stream only with the other side of the disjunction it belongs to, so
the search is a tree of binary interleavings in which deeper
disjunctions receive exponentially smaller shares.  @racket[suspend]
sends the computation to the back of a single, global frontier, which
is closer to breadth-first search.

As a result, answers can come in a different order from the book, and
the first @racket[n] answers returned by @racket[run] can differ even
when all answers eventually appear.  Of the 285 examples of chapters 1
to 9 of the book, 15 differ in this way: 9 return the same answers in
a different order, and 6 return different answers for a bounded
@racket[run]; the book's answers do appear when more answers are
requested.
The global frontier also keeps more partial computations alive, so
programs that search deeply, such as the arithmetic relations of
chapter 8, are noticeably slower than with streams.

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
Gives up control: the rest of the current computation continues after
the computations already waiting in @racket[amb]'s frontier.  It is
implemented with two choice points of one alternative each: the first
is created with @racket[current-amb-depth-first?] set to @racket[#f],
so that the second one, which holds the rest of the computation, is
placed at the back of the frontier.
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
@racket[(amb g1 g2)], and @racket[conj2] is function composition.
}

@deftogether[(@defproc[(ifte [g1 Goal] [g2 Goal] [g3 Goal]) Goal]
              @defproc[(once [g Goal]) Goal])]{
The functions behind @racket[conda] and @racket[condu].  They search
for the answers of @racket[g1] or @racket[g] with @racket[in-amb/do],
so that they can tell whether there is one before committing.
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
