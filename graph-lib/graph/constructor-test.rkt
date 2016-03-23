#lang typed/racket

(module test typed/racket
  (require "constructor.lp2.rkt"
           "../lib/low.rkt"
           "../type-expander/type-expander.lp2.rkt")
  
  (check-equal?: (constructor-values
                  (ann (constructor a 1 "x")
                       ;; TODO: Make a (ConstructorTop …) type expander.
                       (ConstructorTop (List Number String))))
                 (list 1 "x"))
  (check-equal?: (constructor-values
                  (ann (constructor a 1 "x")
                       (ConstructorTop Any)))
                 (list 1 "x"))
  (check-equal?: (constructor-values
                  (ann (constructor a 1 "x")
                       (constructor a Number String)))
                 (list 1 "x")) ;; TODO: test that the tag is 'a
  (check-equal?: (constructor-values
                  (ann (constructor b)
                       (constructor b)))
                 (list)) ;; TODO: test that the tag is 'b
  (check-equal?: (constructor-values
                  (ann (constructor c 'd)
                       (constructor c Symbol)))
                 'd) ;; TODO: test that the tag is 'c
  (check-equal?: (ann (constructor c 2 "y")
                      (constructor c Number String))
                 (constructor c 2 "y"))
  (check-not-equal?: (constructor d 2 "y")
                     (constructor d 2 "y" 'z))
  (check-not-equal?: (constructor e 2 "y")
                     (constructor F 2 "y")))
