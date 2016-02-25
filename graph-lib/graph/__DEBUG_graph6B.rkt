#lang typed/racket

(require "graph-6-rich-returns.lp2.rkt"
         "../lib/low.rkt"
         "graph.lp2.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         "../type-expander/multi-id.lp2.rkt"
         "structure.lp2.rkt" ; debug
         "variant.lp2.rkt" ; debug
         "fold-queues.lp2.rkt"; debug
         "rewrite-type.lp2.rkt"; debug
         "meta-struct.rkt"; debug
         (for-syntax syntax/parse)
         (for-syntax syntax/parse/experimental/template))

(define-syntax (frozen stx)
  (syntax-parse stx
    [(_ a)
     #'(begin
         (define-type-expander (te stx) #'Number)
         (: x (Let [~> te] a))
         (define x 1))]))

(provide frozen)