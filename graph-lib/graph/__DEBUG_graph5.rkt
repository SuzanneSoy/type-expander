#lang typed/racket

#|(require
  "graph.lp2.rkt"
  "../type-expander/type-expander.lp2.rkt")
(define-graph g2
  [a [v : Number] [w : b] ((ma) (a 1 (mb)))]
  [b [v : String] ((mb) (frob) (b "b"))])
(define (frob)
  : (g2 b)
  (error "niy!"))|#

#|
(module mm typed/racket
  (require ;(submod "graph-test.rkt" test)
    "graph.lp2.rkt"
    "../type-expander/type-expander.lp2.rkt")
  
  (provide g2)
  
  (define-graph g2
    [a [v : Number] [w : b] ((ma) (a 1 (mb)))]
    [b [v : String] ((mb) (frob) (b "b"))])
  (define (frob)
    : (g2 b)
    (error "niy!")))

(require "graph.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         'mm)

(λ ([x : (g2 b)]) x)
(λ ([x : (g2 a)]) x)
|#

#|
(require "../type-expander/type-expander.lp2.rkt")
(require "graph.lp2.rkt")

(define-graph g2 [a [v : Number] ((ma) (a 1))])
|#

(require "graph-5-multi-ctors.lp2.rkt"
         "../lib/low.rkt"
         "graph.lp2.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         "../type-expander/multi-id.lp2.rkt"
         (for-syntax syntax/parse))

(define-graph/multi-ctor gm ([a [b1 : b] [b2 : b] [s : String] [v : Number]]
                               [b [a1 : a] [s : String] [v : Number]])
    [(r [v : Integer] [w : String])
     : a
     (printf "r ~a ~a\n" v w)
     (a (bx (if (> v 0) (sub1 v) (string-length w)))
        (by (if (> v 0) (sub1 v) (string-length w)) "xyz")
        w
        v)]
    [(bx [v : Integer])
     : b
     (printf "bx ~a\n" v)
     (b (r v "one") "x" v)]
    [(by [v : Integer] [w : String])
     : b
     (printf "by ~a ~a\n" v w)
     (b (r v "two") "y" (+ v (string-length w)))])

(define gmi (gm 3 "b"))
(check-equal?: (get gmi v) 3)
(check-equal?: (get gmi b1 v) 2)
(check-equal?: (get gmi b1 s) "x")
(check-equal?: (get gmi b1 a1 v) 2)

;(check-equal?: (get gmi b1 a1 b1 a1 v) 1)
;(check-equal?: (get gmi b1 a1 b1 a1 b1 v) 1)
