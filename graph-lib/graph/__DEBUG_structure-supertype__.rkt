#lang typed/racket

(require "../type-expander/type-expander.lp2.rkt")

(define-type-expander (CPairof stx)
  (syntax-case stx ()
    [(_ a) #'(curry Pairof a)]
    [(_ a b) #'(Pairof a b)]))

(ann (ann '(1 . "b") (CPairof Number String))
     (Pairof Number String))

(ann (ann '(1 . "c") ((CPairof Number) String))
     (Pairof Number String))

(require "structure.lp2.rkt")
(require "../type-expander/type-expander.lp2.rkt")

(define ab (structure [f-a 1] [f-b "b"]))
(define abc (structure [f-a 1] [f-b "b"] [f-c 'c]))

(define f
  (λ ([x : (structure-supertype [f-a Number] [f-b String])])
    x))

(f ab)
(f abc)
