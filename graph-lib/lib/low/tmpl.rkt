#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide !each)
  
  (module m-!each racket
    (provide !each)
    (require syntax/parse/experimental/template)
    
    (define-template-metafunction (!each stx)
      (syntax-case stx ()
        [(_ a b) #'b])))
  
  (require 'm-!each))