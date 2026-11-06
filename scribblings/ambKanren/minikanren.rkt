#lang racket/base

;; Typesetting code from the stream-based implementations in
;; miniKanren-typed, so that it links to their documentation.  ambKanren
;; exports many of the same names, with different bindings, so a single
;; for-label require cannot link both.  Each submodule below has the
;; for-label bindings of one module of miniKanren-typed, and its macro
;; gives the code it typesets the lexical context of that submodule:
;; identifiers in (mk1 ...) link to typed/miniKanren/trs1, and those in
;; (mk2 ...) to typed/miniKanren/trs2.

(module recontext racket/base
  (provide recontext)
  ;; Rebuild stx with the lexical context of ctx, keeping the source
  ;; locations (which determine the layout) and the properties.
  (define (recontext ctx stx)
    (let loop ([s stx])
      (define e (syntax-e s))
      (datum->syntax
       ctx
       (cond
         [(pair? e)
          (let loop* ([p e])
            (cond
              [(pair? p) (cons (loop (car p)) (loop* (cdr p)))]
              [(syntax? p) (loop p)]
              [else p]))]
         [(vector? e) (list->vector (map loop (vector->list e)))]
         [(box? e) (box (loop (unbox e)))]
         [else e])
       s
       s))))

(define-syntax-rule (define-typesetter name mod)
  (module name racket/base
    (require scribble/manual
             (for-syntax racket/base (submod ".." recontext))
             (for-label typed/racket/base mod))
    (provide name)
    (define-syntax (name stx)
      (syntax-case stx ()
        [(_ e) (with-syntax ([e* (recontext #'here #'e)])
                 #'(racket e*))]))))

(define-typesetter mk1 typed/miniKanren/trs1)
(define-typesetter mk2 typed/miniKanren/trs2)

(require 'mk1 'mk2)
(provide mk1 mk2)
