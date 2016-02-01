#lang typed/racket

(require "graph.lp2.rkt")
(require "../type-expander/multi-id.lp2.rkt")
(require "../type-expander/type-expander.lp2.rkt")
(define-graph g2 [a [v : Number] ((ma) (a 1))])

(define-multi-id g3
  #:type-expander (λ (stx) #'(List 'x))
  #:else-id g2)

(λ ([x : g3]) x)