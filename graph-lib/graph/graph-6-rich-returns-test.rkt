#lang typed/racket

(module test typed/racket
  (require (for-syntax (submod "graph-6-rich-returns.lp2.rkt" test-syntax)
                       syntax/strip-context))
  
  (define-syntax (insert-tests stx)
    (replace-context stx tests))
  
  (require "graph-6-rich-returns.lp2.rkt"
           "../type-expander/type-expander.lp2.rkt"
           typed/rackunit)
  
  ;(insert-tests);; TODO: FIXME
  )