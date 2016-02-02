#lang typed/racket

(require "../../lib/low.rkt"
         "../../graph/graph.lp2.rkt"
         "../../graph/get.lp2.rkt"
         "../../type-expander/type-expander.lp2.rkt")

(define-graph g2
  [a [v : Number] [ab : b]
     ((ma [arga2 : Integer] [arg3 : String])
      (a arga2 (mb (max 0 (sub1 arga2)))))]
  [b [v : Number] [ba : a]
     ((mb [argb2 : Integer])
      (b argb2 (ma (sub1 argb2) "z")))])

(define gi (g2 3 "b"))
(check-equal?: (get gi v) 3)
(check-equal?: (get gi ab v) 2)
(check-equal?: (get gi ab ba v) 1) ;; should be 1, but was 3 (bug now fixed)

(define-graph g3
  [a [v : Number] [ab : b]
     ((ma [arg : (List 'r Integer String)])
      (a (cadr arg) (mb (list 'b1 (if (> (cadr arg) 0)
                                      (sub1 (cadr arg))
                                      (string-length (caddr arg)))))))]
  [b [v : Number] [ba : a]
     ((mb [arg : (List 'b1 Integer)])
      (b (cadr arg) (ma (list 'r (cadr arg) "z"))))])

(define gi3 (g3 (list 'r 3 "b")))

(check-equal?: (get gi3 v) 3)
(check-equal?: (get gi3 ab v) 2)
(check-equal?: (get gi3 ab ba v) 2) ;; should be 2, but was 3 (bug now fixed)