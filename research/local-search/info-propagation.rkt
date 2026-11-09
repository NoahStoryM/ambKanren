#lang racket/base

;; Experiment A: how `current-amb-info` (amb branch
;; research/amb-tree-info-20261108) propagates.
;; Run: racket research/local-search/info-propagation.rkt
;; Each check prints OBSERVED next to EXPECTED so that surprising
;; behaviour is recorded rather than asserted away.

(require racket/stream racket/list racket/control amb)

(define results '())
(define (record! name expected observed)
  (flush-output)
  (define ok? (equal? expected observed))
  (set! results (cons (list name ok?) results))
  (printf "~a ~a\n    expected ~s\n    observed ~s\n"
          (if ok? "[ok]  " "[DIFF]") name expected observed)
  (flush-output))

(define (tagged tag xs)
  (define pending xs)
  (make-amb (λ () (pair? pending))
            (λ () (define v (car pending)) (set! pending (cdr pending))
               (list v (current-amb-info)))
            tag))

;; A1 nested: info seen *after* an inner choice returns, while still in
;; the outer node's continuation.
(record!
 "A1 info after inner choice is the inner node's (not the outer)"
 '((a outer inner) (a outer inner) (b outer inner) (b outer inner))
 (stream->list
  (in-amb
   (let* ([x (tagged 'outer '(a b))]
          [y (tagged 'inner '(1 2))])
     (list (car x) (cadr x) (current-amb-info))))))

;; A2 DFS/BFS × fair: does info follow the node in every strategy?
(for* ([df '(#t #f)] [fair '(#t #f)])
  (define out
    (parameterize ([current-amb-depth-first? df] [current-amb-fair? fair])
      (stream->list
       (in-amb (amb (tagged 'L '(1 2)) (tagged 'R '(10 20)))))))
  (record! (format "A2 df=~a fair=~a: every value paired with its own tag" df fair)
           #t
           (for/and ([p out])
             (eq? (cadr p) (if (< (car p) 10) 'L 'R)))))

;; A3 parameterize of an UNRELATED parameter inside the search shadows
;; the node info: Racket's parameterize stores the whole
;; parameterization in one continuation mark.
;; EXPECTED here encodes the *observed* behaviour (a documented
;; limitation), not the behaviour one would naively hope for.
(record!
 "A3 parameterize of current-amb-fair? inside in-amb shadows info"
 '((1 #f) (2 #f))
 (stream->list
  (in-amb
   (parameterize ([current-amb-fair? #f])
     (tagged 'node '(1 2))))))

(define p (make-parameter 0))
(record!
 "A3' parameterize of an arbitrary user parameter shadows info"
 '((1 #f) (2 #f))
 (stream->list
  (in-amb (parameterize ([p 1]) (tagged 'node '(1 2))))))

;; A4 failure branch: an inner make-amb whose more? is #f.
(record!
 "A4 failure inside a node does not change the surviving node's info"
 '((8 live))
 (stream->list
  (in-amb
   (let ([x (tagged 'live '(7 8))])
     (when (= (car x) 7) (amb))         ; fail → parent re-enqueued
     x))))

;; A4' a generator whose `get` fails on its first alternative: the
;; failure is reported as #t (parent re-enqueued), the second
;; alternative still runs under the same node binding.
(record!
 "A4' get fails once, next resumption still sees the node info"
 '(after-fail)
 (let ([n 0])
   (stream->list
    (in-amb
     (begin (make-amb (λ () (< n 2))
                      (λ () (set! n (add1 n)) (when (= n 1) (amb)))
                      'after-fail)
            (current-amb-info))))))
;; NOTE (observed while writing this file): a generator whose `more?`
;; is always #t and whose `get` always fails makes the driver loop
;; forever without yielding — the #t "re-enqueue parent" event is not
;; observable from outside the search.  This is the same starvation
;; mechanism as experiment C.

;; A5 continuation capture & re-entry: the same node resumed twice
;; (once per alternative), with an escape continuation captured inside.
(record!
 "A5 info stable across repeated resumption of one node"
 '(n n n)
 (for/list ([v (in-amb/do (tagged 'n '(1 2 3)))]) (cadr v)))

(define seen #f)
;; A6 info is visible to `more?` and `get`?  (`more?` runs inside the
;; resumed continuation, i.e. under the node binding.)
(record!
 "A6 more? and get both observe the node's info"
 '((g g) (g g))
 (let ([n 2])
   (stream->list
    (in-amb
     (make-amb (λ () (begin0 (> n 0) (set! seen (current-amb-info))))
               (λ () (set! n (sub1 n)) (list seen (current-amb-info)))
               'g)))))

;; A7 nested in-amb/do inside a node (the pattern used by ifte/once
;; and by the local disj of experiment B): the inner search's root has
;; info #f, so the *inner* code sees #f, not the enclosing node's info.
(record!
 "A7 inner in-amb/do root sees #f, not the outer node's info"
 '((outer #f))
 (stream->list
  (in-amb
   (let ([x (tagged 'outer '(o))])
     (define-values (more? get)
       (sequence-generate (in-amb/do (current-amb-info))))
     (list (cadr x) (and (more?) (get)))))))

;; A8 info does not identify the producer of an answer: the answer's
;; leaf is reported by the driver, after the node binding is gone.
(record!
 "A8 caller sees its own binding when the leaf comes back"
 '(caller caller)
 (parameterize ([current-amb-info 'caller])
   (for/list ([_ (in-amb/do (tagged 'node '(1 2)))]) (current-amb-info))))

(define bad (filter (λ (r) (not (cadr r))) (reverse results)))
(printf "\n~a checks, ~a as expected, ~a differ: ~s\n"
        (length results) (- (length results) (length bad)) (length bad)
        (map car bad))
(flush-output)
