#lang typed/racket

(require "graph-6-rich-returns.lp2.rkt"
         "../lib/low.rkt"
         "graph.lp2.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         (for-syntax (submod "../type-expander/type-expander.lp2.rkt" expander))
         "../type-expander/multi-id.lp2.rkt"
         "structure.lp2.rkt" ; debug
         "variant.lp2.rkt" ; debug
         "fold-queues.lp2.rkt"; debug
         "rewrite-type.lp2.rkt"; debug
         "meta-struct.rkt"; debug
         racket/splicing
         (for-syntax syntax/parse)
         (for-syntax syntax/parse/experimental/template))

(define-syntax (d-exp stx)
  (syntax-case stx ()
    [(_ a) #'(begin (define x a) x)]))

(define-syntax (frozen stx)
  (syntax-parse stx
    [(_ def val a)
     #`(begin (define def val) ;#,(datum->syntax #'a (syntax->datum #'(define def val)))
              (d-exp a))]))

(define-syntax (goo stx)
  (syntax-case stx ()
    [(_ a)
     #`(frozen #,(datum->syntax #'a 'te) 9
               a)]))

(goo te)

(define-syntax (lake stx)
  (syntax-parse stx
    [(_ val a)
     #`(let ((#,(datum->syntax stx 'tea) val)) a)]))

(lake 3 tea)