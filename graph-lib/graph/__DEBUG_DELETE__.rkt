#lang typed/racket

(require "../lib/debug-syntax.rkt")

(require (for-syntax ;"rewrite-type.lp2.rkt"
          syntax/parse/experimental/template)
         ;"rewrite-type.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt")

#|(define-syntax (flub stx)
  (syntax-case stx ()
    [(_ nam)
     (begin
       (syntax-local-lift-expression #`(browse-syntax #'nam))
       #`(begin
           (: nam Number)
           (define nam 123)
           (void)))]))

(let ((aaa 1))
  (flub aaa))
|#

(define-type-expander (exp stx)
  #'(List 1 2 3))

(define-type e String)
(: x (List e (Let [e exp] e)))
(define x (list "e" (list 1 2 3)))
;(define x (list 0))

(: y (List e))
(define y (list "eee"))
;(define y (list 0))

#|
(define-type-expander (~> stx)
  (syntax-case stx ()
    [_ #'(List 1 2 3)]))

;(λ ([x : (~>)]) x)

(define-syntax (foo stx)
  (template '(U
              (first-step #:placeholder m-cities3/node)
              (tmpl-replace-in-type
               (Listof City)
               (City (first-step #:placeholder City))
               (Street (first-step #:placeholder Street))))))

(foo)
|#