#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb in-amb/do current-amb-depth-first?)
                     typed/microKanren/amb)
          scribble/example
          "utils.rkt"
          "minikanren.rkt")

@(define mk-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/microKanren/amb)))
@(define-syntax-rule (mk-examples body ...)
   (examples #:eval mk-eval body ...))

@title[#:tag "microKanren"]{microKanren}
@defmodule[typed/microKanren/amb #:packages ("ambKanren")]

This module is microKanren (Hemann and Friedman, @italic{μKanren: A
Minimal Functional Core for Relational Programming}, Scheme Workshop
2013) with its streams replaced by @racket[amb].  It follows
@racketmodname[typed/microKanren] of the miniKanren-typed package, the
Typed Racket rewrite of the code at
@url{https://github.com/jasonhemann/microKanren}, definition by
definition; the definitions that only exist to manipulate streams are
gone.

@section[#:tag "microKanren-streams"]{From Streams to amb}

In microKanren, a goal returns a stream of states: every answer at
once, lazily.  The stream type is a monad, and the connectives are its
operations.  With @racket[amb], the nondeterminism is an effect of
evaluation rather than a value, so it disappears from the type of
goals: a goal returns a single state, chooses among its answers with
@racket[amb], and fails by calling @racket[(amb)].

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
(list (list @bold{} @bold{@racketmodname[typed/microKanren]} @bold{@racketmodname[typed/microKanren/amb]})
      (list @racket[Goal]
            @mk0[(→ State (Streamof State))]
            @racket[(→ State State)])
      (list "success" @mk0[(unit s/c)] @racket[s/c])
      (list "failure" @mk0[mzero] @racket[(amb)])
      (list @racket[disj] @mk0[(mplus (g1 s/c) (g2 s/c))] @racket[((amb g1 g2) s/c)])
      (list @racket[conj] @mk0[(bind (g1 s/c) g2)] @racket[(g2 (g1 s/c))])
      (list @racket[Zzz] @mk0[(λ (s/c) (λ () (g s/c)))] @racket[(λ (s/c) (suspend) (g s/c))])
      (list @racket[take] @elem{force and collect the stream @mk0[(g empty-state)]} @elem{collect @racket[(in-amb/do (g empty-state))]}))]

So success returns the state unchanged, conjunction is plain function
composition, and disjunction is a choice made by @racket[amb].  The
monadic structure has not gone away: it now lives in the continuation
captured by @racket[amb], and @racket[in-amb/do] is what turns that
control effect back into a sequence of answers.

The definitions that remain are those of
@racketmodname[typed/microKanren], with these changes:

@itemlist[

@item{@mk0[unit], @mk0[mzero], @mk0[mplus], @mk0[bind],
@mk0[pull], and the stream types are removed.}

@item{@racket[take] and @racket[take-all] take a thunk that performs
the nondeterministic computation, instead of a stream, and
@racket[call/goal] returns a state.}

@item{@racket[Zzz] calls @racket[suspend] (see below).}

]

@subsection[#:tag "microKanren-order"]{Suspension and the Order of Answers}

In microKanren, @mk0[Zzz] wraps a goal so that it returns an immature
stream, and @mk0[mplus] swaps its two streams whenever the first one is
immature; this is what lets a disjunction leave a branch that never
produces an answer.  Here, @racket[Zzz] calls @racket[suspend], which
lets the computations already waiting in @racket[amb]'s frontier run
first:

@mk-examples[
(: relo (→ Term Goal))
(define (relo x) (fresh (a d) (== x `(,a . ,d)) (relo x)))
(run 1 (x) (disj+ (relo '(5 . 6)) (== x 3)))
]

The two schedules produce the same answers but not always in the same
order.  @mk0[mplus] swaps a suspended stream only with the other side
of the disjunction it belongs to, while @racket[suspend] sends the
computation to the back of a single, global frontier, which is closer
to breadth-first search.  All the tests of @tt{microKanren-test.scm}
give the same results as microKanren; of the examples of
@tt{miniKanren-wrappers.scm}, the two that use @tt{rember*o} return
their answers in a different order.

@section[#:tag "microKanren-types"]{Types}

@deftogether[(@deftype[Literal]
              @deftype[Term])]{
@racket[Literal] is the type of atomic values: booleans, numbers,
characters, byte strings, strings, keywords, symbols, and
@racket['()].  @racket[Term] is the type of all terms: literals,
variables, and pairs of terms.
}

@deftogether[(@deftype[Var]
              @defstruct*[var ([c Natural]) #:transparent]
              @defproc[(var=? [x1 Var] [x2 Var]) Boolean])]{
Logic variables.  A variable is identified by its counter @racket[c],
and @racket[var=?] compares counters.
}

@deftogether[(@deftype[Substitution]
              @deftype[State])]{
@racket[Substitution] is defined as @racket[(Listof (Pair Var Term))],
a triangular association list.  @racket[State] is defined as
@racket[(Pair Substitution Natural)]: a substitution and the counter of
the next fresh variable.
}

@deftype[Goal]{
Defined as @racket[(→ State State)].

@mk-examples[
((== 1 1) empty-state)
((call/fresh (λ (q) (== q 5))) empty-state)
(take-all (λ () ((call/fresh (λ (q) (disj (== q 5) (== q 6)))) empty-state)))
]
}

@section[#:tag "microKanren-core"]{The Core}

@defproc[(== [u Term] [v Term]) Goal]{
Unifies @racket[u] and @racket[v] in the substitution of the state,
and calls @racket[(amb)] if they cannot be unified.
}

@defproc[(call/fresh [f (→ Var Goal)]) Goal]{
Applies @racket[f] to a new variable, numbered by the counter of the
state, and runs the resulting goal in a state whose counter is
incremented.
}

@deftogether[(@defproc[(disj [g1 Goal] [g2 Goal]) Goal]
              @defproc[(conj [g1 Goal] [g2 Goal]) Goal])]{
Disjunction runs @racket[g1] or @racket[g2], as chosen by
@racket[amb]; conjunction runs @racket[g2] on the state returned by
@racket[g1].

@mk-examples[
(take-all
 (λ ()
   ((conj (call/fresh (λ (a) (== a 7)))
          (call/fresh (λ (b) (disj (== b 5) (== b 6)))))
    empty-state)))
]
}

@deftogether[(@defproc[(walk [u Term] [s Substitution]) Term]
              @defproc[(ext-s [x Var] [v Term] [s Substitution]) Substitution]
              @defproc[(unify [u Term] [v Term] [s Substitution])
                       (Option Substitution)])]{
Walking, extending a substitution, and unification.  There is no
occurs check.
}

@section[#:tag "microKanren-wrappers"]{User-Level Forms}

These are the forms of @tt{miniKanren-wrappers.scm}.

@defproc[(suspend) Void]{
Sends the rest of the computation to the back of @racket[amb]'s
frontier: it makes a choice point with
@racket[current-amb-depth-first?] set to @racket[#f], and then a
second one, which is placed behind the choices already waiting.
}

@defform[(Zzz goal)]{
The inverse-η-delay: a goal that calls @racket[suspend] before running
@racket[goal], so that a recursive relation gives the other choices a
turn instead of looping.
}

@deftogether[(@defform[(conj+ goal ...+)]
              @defform[(disj+ goal ...+)]
              @defform[(fresh (x ...) goal ...+)]
              @defform[(conde [goal ...+] ...+)])]{
N-ary conjunction and disjunction of suspended goals, fresh variables,
and disjunction of conjunctions.

@mk-examples[
(: appendo (→ Term Term Term Goal))
(define (appendo l s out)
  (conde
    [(== '() l) (== s out)]
    [(fresh (a d res)
       (== `(,a . ,d) l)
       (== `(,a . ,res) out)
       (appendo d s res))]))
(run* (x y) (appendo x y '(1 2 3)))
]
}

@deftogether[(@defform[(run n (x ...+) goal ...+)
                       #:contracts ([n Natural])]
              @defform[(run* (x ...+) goal ...+)])]{
Runs the goals with fresh variables @racket[x ...] and returns at most
@racket[n] (or all) values of the first variable, reified.
}

@deftogether[(@defthing[empty-state State]
              @defproc[(call/goal [g Goal]) State]
              @defproc[(take [n Natural] [th (→ a)]) (Listof a)]
              @defproc[(take-all [th (→ a)]) (Listof a)])]{
Running goals: @racket[call/goal] runs @racket[g] in
@racket[empty-state], and @racket[take] and @racket[take-all] collect
at most @racket[n] (or all) results of @racket[(th)] with
@racket[in-amb/do].
}

@deftogether[(@defproc[(reify-1st [s/c State]) Term]
              @defproc[(walk* [v Term] [s Substitution]) Term]
              @defproc[(reify-s [v Term] [s Substitution]) Substitution]
              @defproc[(reify-name [n Natural]) Symbol])]{
Reification of the value of the first variable, with the names
@racket['_.0], @racket['_.1], and so on.
}

@defproc[(fresh/nf [n Natural] [f (→ Var * Goal)]) Goal]{
Applies @racket[f] to @racket[n] fresh variables.
}
