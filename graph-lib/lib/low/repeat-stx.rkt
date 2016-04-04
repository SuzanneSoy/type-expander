#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide repeat-stx)
  
  (require syntax/stx
           (for-syntax racket/base
                       racket/syntax
                       syntax/parse))
  
  (define-for-syntax (repeat-stx-2 stx)
    (syntax-parse stx
      [(a:id b:id)
       #'(λ _ a)]
      [(a:id (b:expr (~literal ...)))
       #`(λ (bs) (stx-map #,(repeat-stx-2 #'(a b)) bs))]))
  
  (define-for-syntax (repeat-stx-1 stx)
    (syntax-parse stx
      [(a:id b:expr)
       #`(λ (a bs) (#,(repeat-stx-2 #'(a b)) bs))]
      [((a:expr (~literal ...)) (b:expr (~literal ...)))
       #`(λ (s1 s2) (stx-map #,(repeat-stx-1 #'(a b)) s1 s2))]))
  
  (define-syntax (repeat-stx stx)
    (syntax-parse stx
      [(_ a:expr b:expr)
       #`(#,(repeat-stx-1 #'(a b)) #'a #'b)])))

(module test racket
  (require (submod ".." untyped))
  (require syntax/parse
           rackunit)
  
  (check-equal?
   (syntax-parse #'(1 2)
     [(a b)
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx a b)))])
   1)
  
  (check-equal?
   (syntax-parse #'(1 2 3)
     [(a b ...)
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx a (b ...))))])
   '(1 1))
  
  (check-equal?
   (syntax-parse #'(1 (2 3) (uu vv ww) (xx yy))
     [(a (b ...) ...)
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx a ((b ...) ...))))])
   '((1 1) (1 1 1) (1 1)))
  
  (check-equal?
   (syntax-parse #'(1 ((2) (3 3)) ((uu) (vv vv) (ww ww ww)) ((xx) (yy)))
     [(a ((b ...) ...) ...)
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx a (((b ...) ...) ...))))])
   '(((1) (1 1)) ((1) (1 1) (1 1 1)) ((1) (1))))
  
  (check-equal?
   (syntax-parse #'([1 x] [2 y] [3 z])
     [([a b] ...)
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx (a ...) (b ...))))])
   '(1 2 3))
  
  (check-equal?
   (syntax-parse #'((1 2 3) (a b))
     [([a b ...] ...)
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx (a ...) ((b ...) ...))))])
   '((1 1) (a)))
  
  (check-equal?
   (syntax-parse #'(((1 2 3) (a b)) ((x y z t) (-1 -2)))
     [[[[a b ...] ...] ...]
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx ((a ...) ...) (((b ...) ...) ...))))])
   '(((1 1) (a)) ((x x x) (-1))))
  
  (check-equal?
   (syntax-parse #'((f (1 2 3) (a b)) (g (x y z t) (-1 -2)))
     [[[a (b ...) ...] ...]
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx (a ...) (((b ...) ...) ...))))])
   '(((f f f) (f f)) ((g g g g) (g g))))
  
  (check-equal?
   (syntax-parse #'((h () ()) (i () (x y z) ()))
     [([a (b ...) ...] ...)
      (syntax->datum
       (datum->syntax
        #'dummy
        (repeat-stx (a ...) (((b ...) ...) ...))))])
   '((() ()) (() (i i i) ()))))