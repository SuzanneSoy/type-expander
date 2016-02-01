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

;#|
#|
(module mm typed/racket
  (require ;(submod "graph.lp2.rkt" test)
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

(require "graph-5-multi-ctors.lp2.rkt")
(require "../lib/low.rkt"
         "graph.lp2.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         "../type-expander/multi-id.lp2.rkt")

(define-graph/multi-ctor gm ([a [b1 : b] [b2 : b] [v : Number]]
                             [b [a : a] [s : String] [v : Number]])
  [(r [v : Number] [w : String])
   : a
   (a (bx (if (> 0 v) (sub1 v) (+ v (string-length w))))
      (by (if (> 0 v) (sub1 v) (+ v (string-length w))) "xyz")
      v)]
  [(bx [v : Number])
   : b
   (b (r v) "x" v)]
  [(by [v : Number] [w : String])
   : b
   (b (r v) "y" (+ v (string-length w)))])


















