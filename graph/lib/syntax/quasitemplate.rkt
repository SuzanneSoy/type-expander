#lang racket

(require syntax/parse/experimental/template
         (for-syntax syntax/parse
                     racket/syntax))

(provide quasitemplate
         (all-from-out syntax/parse/experimental/template))

;; subst-quasitemplate returns a stx-pair, with definitions for
;; with-syntax in the stx-car, and a template in the stx-cdr.
;; The template is either of the form ('eh-tmpl . tmpl), in which case it is an
;; ellipsis-head template, or of the form ('tmpl . tmpl), in which case it is
;; a regular template.

;; Appending the stx-car from the two branches at each recursion step is
;; extremely inefficient (in the worst case O(n²)), so while gathering them, we
;; store them as a binary tree, and then we flatten it with flatten-defs.

;; Note that quasitemplate can still take O(n²) time, because of ellipsis-head
;; templates which are not handled very efficiently.

(define-for-syntax (flatten-defs stx acc)
  (syntax-parse stx
    [(l r) (flatten-defs #'r (flatten-defs #'l acc))]
    [() acc]
    [(def) #`(def . #,acc)]))

;; There are two cases for the transformation of #,@(expr):
;; If it is in a car position, we write:
;;   (with-syntax ([(tmp ...) expr]) (tmp ... . the-cdr))
;; If it is in a cdr position, we write:
;;   (with-syntax ([tmp expr]) (the-car . tmp))
(define-for-syntax (subst-quasitemplate car? stx)
  (syntax-parse stx #:literals (unsyntax unsyntax-splicing)
    [(unsyntax expr)
     (with-syntax ([tmp (gensym)])
       #`(([tmp expr]) . #,(if car? #'{tmp} #'tmp)))]
    [(unsyntax-splicing expr)
     (with-syntax ([tmp (gensym)])
       (if car?
           #'(... (([(tmp ...) expr]) . {tmp ...}))
           #'(([tmp expr]) . tmp)))]
    [((unsyntax-splicing expr)) ;; In last position in a list
     (if car?
         #'(([tmp expr]) . {tmp})
         #'(([tmp expr]) . tmp))]
    [(a . b)
     (with-syntax ([(defs-a sa ...) (subst-quasitemplate #t #'a)]
                   [(defs-b . sb) (subst-quasitemplate #f #'b)])
       #`((defs-a defs-b) . #,(if car? #'{(sa ... . sb)} #'(sa ... . sb))))]
    [x
     #`(() . #,(if car? #'{x} #'x))]))

(define-syntax (quasitemplate stx)
  (syntax-parse stx
    [(_ tmpl)
     (with-syntax* ([(defs . new-tmpl) (subst-quasitemplate #f #'tmpl)]
                    [(flattened-defs ...) (flatten-defs #'defs #'())])
       #'(with-syntax (flattened-defs ...)
           (template new-tmpl)))]))

(module+ test
  (require rackunit)
  (define-syntax-rule (check . tmpl)
    (check-equal? (syntax->datum (quasitemplate . tmpl))
                  (syntax->datum (quasisyntax . tmpl))))
  
  (check (a #,(+ 1 2)))
  (check (a #,(+ 1 2) #,(+ 3 4)))
  (check (a #,@(list 1 2) #,@(list 3 4)))
  (check (#,@(list 1 2) #,@(list 3 4) . #,(list* 5 6)))
  (check (a (#,@(list 1 2) #,@(list 3 4) . #,(list* 5 6)) c))
  (check (a . (#,@(list 1 2) #,@(list 3 4) . #,(list* 5 6))))
  (check (a (#,@(list 1 2) #,@(list 3 4) . #,(list* 5 6))))
  
  (check (#,@(list 1 2) #,@(list 3 4) #,@(list* 5 6)))
  (check (a (#,@(list 1 2) #,@(list 3 4) #,@(list* 5 6)) c))
  (check (a . (#,@(list 1 2) #,@(list 3 4) #,@(list* 5 6))))
  (check (a (#,@(list 1 2) #,@(list 3 4) #,@(list* 5 6))))
  (check (a #,@1))
  (check (a (#,@1)))
  (check (a (#,@1) c))
  (check ((#,@1) b))
  (check ((#,@1) b)))