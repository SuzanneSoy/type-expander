#lang typed/racket

(require #|"graph-6-rich-returns.lp2.rkt"
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
         "../lib/debug-syntax.rkt"
         racket/splicing|#
         (for-syntax syntax/parse)
         (for-syntax syntax/parse/experimental/template))

;(syntax-local-lift-expression #`(browse-syntaxes (list #'e #'b)))
(define-syntax (d-exp stx)
  (syntax-parse stx
    [(_ (_ (e) _) b)
     (displayln (free-identifier=? #'e #'b))
     #'(void)]))

(define-syntax (frozen stx)
  (syntax-parse stx
    [(_ def b)
     #`(begin def ;#,(datum->syntax #'a (syntax->datum #'(define def val)))
              (d-exp def b))]))

(define-syntax (goo stx)
  (syntax-parse stx
    [(_ b)
     (define i1 (make-syntax-delta-introducer #'te #'b))
     (define i2 (make-syntax-delta-introducer #'b #'te))
     #`(frozen (define (#,(i2 #'te)) 1)
               #,(i1 #'b))]))

(provide goo)