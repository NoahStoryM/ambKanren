#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb amb-fail in-amb/do amb-generate* amb-local amb-pause current-amb-merger)
                     (only-in amb/merge amb-interleave amb-alternate amb-level amb-append)
                     typed/microKanren/amb)
          scribble/example
          "utils.rkt"
          "minikanren.rkt")

@(define mk-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require typed/microKanren/amb typed/amb typed/amb/merge)
                   '(current-amb-merger amb-interleave)))
@(define-syntax-rule (mk-examples body ...)
   (examples #:eval mk-eval body ...))

@title[#:tag "microKanren"]{microKanren}
@defmodule[typed/microKanren/amb #:packages ("ambKanren")]

This module is microKanren (Hemann and Friedman, @italic{μKanren: A
Minimal Functional Core for Relational Programming}, Scheme Workshop
2013) with its streams replaced by @racket[amb].  It follows, definition
by definition, @racketmodname[typed/microKanren] of the miniKanren-typed
package, which is the Typed Racket version of the code at
@url{https://github.com/jasonhemann/microKanren}.  The definitions that
only manipulate streams are gone.

@section[#:tag "microKanren-streams"]{From Streams to amb}

In microKanren, a goal returns a stream of states: all its answers,
produced lazily.  Conjunction uses stream bind to run one goal on
each answer of another.  With @racket[amb], a goal has a single state
as its return type, chooses among its answers with @racket[amb], and
fails by calling @racket[(amb-fail)].

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
(list (list @bold{} @bold{@racketmodname[typed/microKanren]} @bold{@racketmodname[typed/microKanren/amb]})
      (list @racket[Goal]
            @mk0[(→ State (Streamof State))]
            @racket[(→ State State)])
      (list "success" @mk0[(unit s/c)] @racket[s/c])
      (list "failure" @mk0[mzero] @racket[(amb-fail)])
      (list @racket[disj] @mk0[(mplus (g1 s/c) (g2 s/c))] @racket[((amb g1 g2) s/c)])
      (list @racket[conj] @mk0[(bind (g1 s/c) g2)] @racket[(g2 (amb-local (g1 s/c)))])
      (list @racket[Zzz] @mk0[(λ (s/c) (λ () (g s/c)))] @racket[(λ (s/c) (amb-pause) (g s/c))])
      (list @racket[take] @elem{force and collect the stream @mk0[(g empty-state)]} @elem{advance @racket[amb-generate*] until enough answers arrive}))]

Success returns the state unchanged, disjunction is a choice made by
@racket[amb], and conjunction runs @racket[g2] on each answer of a
local search of @racket[g1].  @racket[amb-generate*] provides a
generator of answers and pauses.  @racket[amb-local] generates the
events of @racket[g1] on demand, supplying each answer as an
alternative of one choice that continues in @racket[g2].  This
implements the same scheduling as @mk0[bind] with the stream of
@racket[g1] under @racket[amb-interleave] and the default shuffler.

The definitions that remain are those of
@racketmodname[typed/microKanren], with these changes:

@itemlist[

@item{@mk0[unit], @mk0[mzero], @mk0[mplus], @mk0[bind],
@mk0[pull], and the stream types are removed.}

@item{@racket[take] and @racket[take-all] take a thunk that performs
the nondeterministic computation, instead of a stream, and
@racket[call/goal] returns a state.}

@item{@racket[unify] calls @racket[(amb-fail)] instead of returning
@racket[#f], so it returns a substitution, and @racket[==] needs no
test.}

@item{@racket[Zzz] calls @racket[amb-pause] (see below).}

]

@subsection[#:tag "microKanren-order"]{Suspension and the Order of Answers}

In microKanren, @mk0[Zzz] creates a suspended stream.  Here it calls
@racket[amb-pause] before the goal.  The wrapper forms that use
@racket[Zzz] inherit this pause.  Calling a goal directly adds no
pause of its own.  The default merger of amb,
@racket[amb-append], does not pass the turn at a pause, so the
examples select @racket[amb-interleave] explicitly.

@mk-examples[
(: relo (→ Term Goal))
(define (relo x) (fresh (a d) (== x `(,a . ,d)) (relo x)))
(run 1 (x) (disj+ (relo '(5 . 6)) (== x 3)))
]

With @racket[amb-interleave] and the default shuffler, conjunction
uses a local search to follow the answer order of the stream-based
implementation.  Plain function composition, @racket[(g2 (g1 s/c))],
runs @racket[g2] inside the choices of @racket[g1], so the merger
interleaves the runs of @racket[g2] following the shape of those
choices.  @mk0[bind] interleaves them in the order of the answers of
@racket[g1], with the pauses of @racket[g1] in between.  When both
versions completely enumerate a pure search without pruning, they
produce the same answers in possibly different orders.  A bounded
@racket[run] observes the difference.

Stream bind and @racket[conj] can both change the order of answers
when conjunctions are regrouped.  Conjunction is therefore not
associative under equality of ordered answers.

@mk-examples[
(: association-query (→ Boolean (Listof Term)))
(define (association-query left?)
  (run 2 (q x y z)
    (== q (list x y z))
    (let ([a (disj (== x 'a) (== x 'b))]
          [b (disj (== y 'l) (== y 'r))]
          [c (disj (== z 'u) (Zzz (== z 'v)))])
      (if left? (conj (conj a b) c) (conj a (conj b c))))))
(association-query #t)
(association-query #f)
]

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
and calls @racket[(amb-fail)] if they cannot be unified.
}

@defproc[(call/fresh [f (→ Var Goal)]) Goal]{
Applies @racket[f] to a new variable, numbered by the counter of the
state, and runs the resulting goal in a state whose counter is
incremented.
}

@deftogether[(@defproc[(disj [g1 Goal] [g2 Goal]) Goal]
              @defproc[(conj [g1 Goal] [g2 Goal]) Goal])]{
Disjunction runs @racket[g1] or @racket[g2], as chosen by
@racket[amb]; conjunction runs @racket[g2] on each state that a local
search of @racket[g1] returns, as @racket[(g2 (amb-local (g1 s/c)))].

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
              @defproc[(unify [u Term] [v Term] [s Substitution]) Substitution])]{
Walking, extending a substitution, and unification.  There is no
occurs check.  @racket[unify] returns the resulting substitution on
success.  When @racket[u] and @racket[v] cannot be unified,
@racket[unify] calls @racket[(amb-fail)].  Call it within a search to
handle that case as search failure.
}

@section[#:tag "microKanren-wrappers"]{User-Level Forms}

These are the forms of @tt{miniKanren-wrappers.scm}.

@defform[(Zzz goal)]{
The inverse-η-delay: a goal that calls @racket[amb-pause] before running
@racket[goal], so that a recursive relation gives the other
alternatives a turn instead of looping.
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
Running goals.  @racket[call/goal] runs @racket[g] in
@racket[empty-state].  @racket[take] and @racket[take-all] collect at
most @racket[n] (or all) results of @racket[(th)] with
@racket[amb-generate*]; they skip pauses only there, at the outermost
level, so the pauses still schedule the search inside.
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
