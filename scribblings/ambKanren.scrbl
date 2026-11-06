#lang scribble/manual

@(require (for-label typed/racket/base
                     (only-in amb amb in-amb/do current-amb-fair?)
                     ambKanren)
          scribble/example
          "utils.rkt")

@(define ak-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require ambKanren typed/amb)))
@(define-syntax-rule (ak-examples body ...)
   (examples #:eval ak-eval body ...))

@(define demo-eval
   (make-base-eval #:lang 'typed/racket/base
                   '(require ambKanren/demo)))

@title{ambKanren: miniKanren Based on amb}
@defmodule[ambKanren #:packages ("ambKanren")]
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

ambKanren is a miniKanren in which backtracking is not implemented by a
stream of answers but delegated to the ambiguous operator @racket[amb]
of the @racketmodname[typed/amb] library.  A goal is an ordinary
function from substitutions to substitutions; when a goal fails it
calls @racket[(amb)], and when it has several answers it chooses one of
them with @racket[amb].  The search over all choices is performed by
@racket[in-amb/do], which @racket[run] uses to collect answers.

@section[#:tag "basis"]{Implementation Basis}

The core (@racket[walk], @racket[unify], binary @racket[disj] and
@racket[conj], @racket[Zzz], @racket[conj+] and @racket[disj+], and
reification) follows microKanren (Hemann and Friedman, @italic{μKanren:
A Minimal Functional Core for Relational Programming}, 2013), with the
stream operations replaced by @racket[amb].  The module
@racketmodname[ambKanren/demo] is a minimal version of this core; see
@secref["demo"].

The user-level language provided by @racketmodname[ambKanren] is that
of the @emph{first} edition of @italic{The Reasoned Schemer}
(Friedman, Byrd, and Kiselyov, MIT Press, 2005): @racket[run] with a
single query variable, @racket[conde] with @racket[else], the
interleaving forms @racket[condi] and @racket[alli], and the
committed-choice forms @racket[conda] and @racket[condu].  ``Chapter
@italic{n}'' below refers to that edition.

The differences from these sources are:

@itemlist[

@item{There is no stream type and no fresh-variable counter.
Variables are created directly by @racket[fresh], as in the book,
rather than by microKanren's @tt{call/fresh} on a state.}

@item{@racket[ext-s] performs the occurs check; failure is signalled by
@racket[(amb)] rather than by returning @racket[#f].}

@item{Substitutions are immutable hash tables in
@racketmodname[ambKanren] and association lists in
@racketmodname[ambKanren/demo].}

@item{@racket[conde] is depth-first, as in the first edition: it
corresponds to unfair @racket[amb].  The interleaving forms
@racket[condi] and @racket[alli] are implemented separately (see
@secref["interleaving"]); the order in which they produce answers is
not always the order given in the book.}

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
      (list @racket[run] "take answers from the stream (g empty-s)" @elem{take answers from @racket[(in-amb/do (g empty-s))]}))]

So @racket[succeed] is the identity function, conjunction is plain
function composition, and disjunction is a choice made by
@racket[amb].  The monadic structure has not gone away: it now lives in
the continuation captured by @racket[amb], and @racket[in-amb/do] is
what turns that control effect back into a sequence of answers.

@section[#:tag "types"]{Types}

@deftype[Term]{
The type of terms: booleans, numbers, characters, byte strings,
strings, keywords, symbols, @racket['()], logic variables
(@racket[Var]), and pairs of terms.
}

@deftogether[(@deftype[Var]
              @defstruct*[var ([name Symbol]) #:transparent]
              @defproc[(var=? [x Var] [y Var]) Boolean])]{
Logic variables.  Variables are compared by identity
(@racket[var=?] is @racket[eq?]); the name is only for printing.
}

@deftype[Substitution]{
Defined as @racket[(Immutable-HashTable Var Term)], with
@racket[eq?]-based keys.  As in other miniKanren implementations, the
substitution is triangular: a variable may be associated with a term
containing other associated variables.
}

@deftype[Goal]{
Defined as @racket[(→ Substitution Substitution)].  Applying a goal to
a substitution returns one extension of it under which the goal holds,
chosen with @racket[amb]; the other extensions are explored when the
search backtracks.  A goal with no answer calls @racket[(amb)].  Goals
must therefore be applied within the dynamic extent of an
@racket[amb] search such as @racket[in-amb/do].

@ak-examples[
(define x (var 'x))
(for/list : (Listof Term)
          ([s : Substitution
              (in-amb/do ((disj (== x 1) (== x 2)) empty-s))])
  (walk x s))
]
}

@section[#:tag "interface"]{Running Programs}

@deftogether[(@defform[(run n (x) goal ...)
                       #:contracts ([n (Option Real)])]
              @defform[(run* (x) goal ...)])]{
Introduces a fresh variable @racket[x], applies the conjunction of the
@racket[goal]s to the empty substitution inside @racket[in-amb/do],
and returns a list of at most @racket[n] reified values of
@racket[x].  When @racket[n] is @racket[#f], all values are returned,
and the computation diverges if there are infinitely many.  The limit
is checked before each new answer is requested, so @racket[run]
never searches for an answer it will not return.  (Chapter 1.)

@ak-examples[
(run* (q) (fresh (a b) (== q (list a b)) (== a 'left)))
]
}

@section[#:tag "goals"]{Goals}

@deftogether[(@defthing[succeed Goal]
              @defthing[fail Goal])]{
@racket[succeed] is the identity function; @racket[fail] calls
@racket[(amb)].  (Chapter 1.)
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
@racket[(all goal ...)].  The variables are created each time the goal
is applied.  (Chapter 1.)
}

@defform[(project (x ...) goal ...)]{
Rebinds each @racket[x] to its value under the current substitution
(as by @racket[walk*]) and then behaves as @racket[(all goal ...)].
This lets the goals inspect the values with ordinary Racket code.
(Chapter 9.)

@ak-examples[
(run* (q)
  (fresh (n)
    (conde [(== n 3)] [(== n 4)])
    (project (n)
      (if (exact-integer? n) (== q (* n n)) fail))))
]
}

@deftogether[(@defproc[(disj [g Goal] ...) Goal]
              @defproc[(conj [g Goal] ...) Goal])]{
N-ary disjunction and conjunction of goals.  @racket[disj] chooses one
of the goals with @racket[amb] and applies it; @racket[conj] applies
the goals in order, each to the result of the previous one.  Both
simplify trivial cases: @racket[fail] is dropped from a disjunction, a
conjunction containing @racket[fail] is @racket[fail], and
@racket[succeed] is dropped from a conjunction.

These are functions, so their arguments are evaluated before the goal
is built.  For recursive relations, use @racket[disj+] and
@racket[conj+] or the forms below.
}

@deftogether[(@defform[(Zzz goal)]
              @defform[(λG (s) body ...+)])]{
@racket[(Zzz goal)] is the η-expansion @racket[(λG (s) (goal s))]: it
delays the evaluation of the expression @racket[goal] until the goal is
applied, which keeps recursive relations from looping while they are
being built.  Unlike microKanren's @tt{Zzz}, it does not suspend a
stream, since there is none.  @racket[λG] is @racket[λ] annotated with
the type @racket[Goal].
}

@deftogether[(@defform[(disj+ goal ...)]
              @defform[(conj+ goal ...)])]{
@racket[disj] and @racket[conj] applied to @racket[(Zzz goal) ...].
}

@deftogether[(@defform[(all goal ...)]
              @defform[(alli goal ...)])]{
Conjunction.  @racket[(all)] is @racket[succeed] and
@racket[(all goal)] is @racket[(Zzz goal)]; otherwise @racket[all] is
@racket[conj+] and @racket[alli] is @racket[conji+].  (Chapter 6.)
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
Committed choice: the first goal of each clause is a test, and the
first clause whose test succeeds is the only one used.  @racket[conda]
continues with every answer of the test, @racket[condu] with its first
answer only.  (Chapter 10.)

@ak-examples[
(run* (q) (conda [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
(run* (q) (condu [(conde [(== q 1)] [(== q 2)])] [(== q 3)]))
]
}

@deftogether[(@defform[(ife g0 g1 g2)]
              @defform[(ifi g0 g1 g2)]
              @defform[(ifa g0 g1 g2)]
              @defform[(ifu g0 g1 g2)]
              @defform[(cond-aux ifer clause ...)]
              @defform[(all-aux conj+ goal ...)]
              @defform[(if-aux disj+ g0 g1 g2)])]{
The building blocks of the forms above, following the structure of the
first edition's implementation: @racket[cond-aux] turns each clause
into @racket[(ifer test (all goal ...) rest)], and @racket[all-aux]
builds a conjunction from a conjunction form.  @racket[ifa] and
@racket[ifu] obtain the answers of the test with
@racket[sequence-generate] over @racket[in-amb/do], so that they can
tell whether it has an answer before committing.
}

@subsection[#:tag "interleaving"]{Interleaving}

@deftogether[(@defproc[(disji [g Goal] ...) Goal]
              @defproc[(conji [g Goal] ...) Goal]
              @defform[(disji+ goal ...)]
              @defform[(conji+ goal ...)])]{
Interleaving disjunction and conjunction, used by @racket[condi] and
@racket[alli].  @racket[disji] collects the answers of each goal as a
sequence with @racket[in-amb/do], merges the sequences round-robin
with @racket[mplusi], and chooses from the merged sequence with
@racket[amb].  @racket[conji] threads the answers through the goals,
choosing among the answers of each goal with
@racket[current-amb-fair?] enabled.

@bold{Known limitation:} @racket[conji] is not fair when an earlier
goal has infinitely many answers: in
@racket[(alli (counto n) (conde [(== side 'L)] [(== side 'R)]))], where
@racket[counto] has an answer for every length, no answer with
@racket['R] is ever produced.  The first-edition @racket[alli] does
produce them.
}

@deftogether[(@defproc[(mplusi [s* (Sequenceof Substitution)] ...)
                       (Sequenceof Substitution)]
              @defproc[(bindi [s* (Sequenceof Substitution)] [g Goal])
                       (Sequenceof Substitution)])]{
Sequence-level helpers.  @racket[mplusi] merges sequences of
substitutions round-robin; @racket[bindi] applies @racket[g] to each
element of @racket[s*] and merges the results.
}

@section[#:tag "substitutions"]{Substitutions and Unification}

@deftogether[(@defthing[empty-s Substitution]
              @defproc[(s-empty? [s Substitution]) Boolean]
              @defproc[(s-has-key? [x Var] [s Substitution]) Boolean]
              @defproc[(apply-s [s Substitution] [x Var]) Term]
              @defproc[(size-s [s Substitution]) Index]
              @defproc[(ext-s [x Var] [v Term] [s Substitution]) Substitution])]{
Operations on substitutions.  @racket[ext-s] associates @racket[x]
with @racket[v], and fails with @racket[(amb)] when @racket[x] occurs
in @racket[v].
}

@deftogether[(@defproc[(walk [v Term] [s Substitution]) Term]
              @defproc[(walk* [v Term] [s Substitution]) Term]
              @defproc[(occurs? [x Var] [v Term] [s Substitution]) Boolean]
              @defproc[(unify [u Term] [v Term] [s Substitution]) Substitution])]{
@racket[walk] follows the associations of a variable until it reaches
a term that is not an associated variable; @racket[walk*] does so
throughout a term.  @racket[unify] returns an extension of @racket[s]
that makes @racket[u] and @racket[v] equal, and calls @racket[(amb)]
if there is none.  (Chapter 9.)
}

@deftogether[(@defproc[(reify-name [n Index]) Symbol]
              @defproc[(reify-s [v Term] [s Substitution]) Substitution]
              @defproc[(reify [v Term]) Term])]{
Reification: unassociated variables are replaced by @racket['_.0],
@racket['_.1], and so on, in order of first appearance.  (Chapter 9.)
}

@deftogether[(@defproc[(apply-goal [g Goal] [s Substitution]) Substitution]
              @defproc[(sequence->amb [s (Sequenceof a)]) a]
              @defproc[(list->amb [l (Listof a)]) a])]{
Small helpers: @racket[apply-goal] applies a goal (for use with
@racket[foldl]); @racket[sequence->amb] and @racket[list->amb] choose
an element with @racket[amb].
}

@section[#:tag "aliases"]{Symbolic Aliases}

@deftogether[(@defidform[run∞]
              @defidform[⊤]
              @defidform[⊥]
              @defidform[≡]
              @defidform[∃]
              @defidform[∨]
              @defidform[∧]
              @defidform[∨i]
              @defidform[∧i])]{
Aliases for @racket[run*], @racket[succeed], @racket[fail],
@racket[==], @racket[fresh], @racket[disj+], @racket[conj+],
@racket[disji+], and @racket[conji+].

@ak-examples[
(run∞ (q) (∃ (a b) (∨ (≡ a 1) (≡ a 2)) (≡ b a) (≡ q (list a b))))
]
}

@section[#:tag "demo"]{A Minimal Version}

@defmodule[ambKanren/demo]

This module is a self-contained, minimal version of the core, close in
size and structure to microKanren: substitutions are association lists,
@racket[disj] and @racket[conj] are binary, and the user-level forms
are @racket[run], @racket[run*], @racket[fresh], @racket[all], and
@racket[conde].  It is intended for reading rather than for use: every
definition is a few lines long, and the whole correspondence of
@secref["correspondence"] can be seen at once.

The module exports the same names as the corresponding parts of
@racketmodname[ambKanren] (@racket[Term], @racket[Substitution],
@racket[Goal], @racket[var], @racket[walk], @racket[unify],
@racket[==], @racket[disj], @racket[conj], @racket[succeed],
@racket[fail], @racket[Zzz], @racket[disj+], @racket[conj+],
@racket[reify], and so on), so the two modules should not be required
together.

@examples[#:eval demo-eval
(run* (q) (fresh (a b) (conde [(== a 1)] [(== a 2)]) (== b a) (== q (cons a b))))
]
