#lang typed/racket

(module test typed/racket
  (require (submod "graph-test.rkt" test))
  (require "get.lp2.rkt")
  (require "adt.lp2.rkt")
  (require "../lib/low.rkt")
  (require "../type-expander/type-expander.lp2.rkt")
  
  (check-equal?: (get '((1 2) (3)) … …)
                 '((1 2) (3)))
  
  (uniform-get g people)
  (get g people)
  (get g streets cadr houses car owner name)
  ((λget people) g)
  (check-equal?: ((λget owner name) (get g streets cadr houses car))
                 "Jack")
  (check-equal?: (get g streets … houses … owner name)
                 '(("Amy" "Anabella") ("Jack")))
  (check-equal?: ((λget streets … houses … owner name) g)
                 '(("Amy" "Anabella") ("Jack")))
  (check-true: (procedure? (let ([f (λget streets … houses … owner name)]) f))))