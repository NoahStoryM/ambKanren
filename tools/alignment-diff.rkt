#lang racket/base

;; Emit source-preserving top-level comparisons; Python's difflib can render
;; the JSON differences. #lang, comments between forms and imports are omitted.
(require json racket/file racket/list racket/port racket/runtime-path)
(define-runtime-path repo "..")
(define reference (string->path (vector-ref (current-command-line-arguments) 0)))

(define (head-name value)
  (if (pair? value) (head-name (car value)) value))
(define (form-key form)
  (and (pair? form)
       (let ([head (car form)])
         (and (memq head '(define define-type struct define-predicate
                          define-syntax define-syntax-rule :))
              (format "~a:~a"
                      (if (eq? head 'define-syntax-rule) 'define-syntax head)
                      (head-name (cadr form)))))))
(define (forms paths)
  (define result (make-hash))
  (for ([path (in-list paths)])
    (define source (file->string path))
    (call-with-input-file path
      (λ (in)
        (port-count-lines! in)
        (read-line in) ; #lang
        (let loop ()
          (define stx (read-syntax path in))
          (unless (eof-object? stx)
            (define key (form-key (syntax->datum stx)))
            (when key
              (when (hash-has-key? result key) (error 'forms "duplicate ~a" key))
              (hash-set! result key
                         (substring source (sub1 (syntax-position stx))
                                    (+ (sub1 (syntax-position stx)) (syntax-span stx)))))
            (loop))))))
  result)
(define (paths root names)
  (map (λ (name) (build-path root name)) names))
(define comparisons
  (list
   (list "trs1" '("miniKanren/trs1/main.rkt")
         '("miniKanren/amb/trs1/private/types.rkt"
           "miniKanren/amb/trs1/base.rkt" "miniKanren/amb/trs1/main.rkt"))
   (list "trs2" '("miniKanren/trs2/main.rkt") '("miniKanren/amb/trs2/main.rkt"))
   (list "microKanren" '("microKanren/main.rkt") '("microKanren/amb/main.rkt"))))
(write-json
 (for/list ([comparison (in-list comparisons)])
   (define native (forms (paths reference (cadr comparison))))
   (define amb (forms (paths repo (caddr comparison))))
   (hasheq 'edition (car comparison)
           'forms
           (for/list ([key (in-list (sort (remove-duplicates
                                          (append (hash-keys native) (hash-keys amb))) string<?))])
             (define left (hash-ref native key #f))
             (define right (hash-ref amb key #f))
             (hasheq 'key key 'identical (equal? left right)
                     'reference left 'amb right)))))
(newline)
