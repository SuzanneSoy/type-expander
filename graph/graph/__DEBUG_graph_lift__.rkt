#lang typed/racket

(define-syntax (l stx)
  #`(let ()
      (syntax-local-lift-expression (define-type Foo (List 'foo Foo Bar One)))
      (syntax-local-lift-expression (define-type Bar (List 'foo Foo Bar One)))
      1))

(l)