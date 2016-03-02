#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules
  (provide require/provide)
  
  (define-syntax (require/provide stx)
    (syntax-case stx ()
      [(_ require-spec ...)
       #'(begin
           (require require-spec ...)
           (provide (all-from-out require-spec ...)))]))
  
  (module+ test
    (require typed/rackunit)
    (module ma typed/racket
      (define require-provide-foo 7)
      (provide require-provide-foo))
    (module mb typed/racket
      (require (submod ".." ".."))
      (require/provide (submod ".." ma)))
    (require 'mb)
    (check-equal? require-provide-foo 7)))