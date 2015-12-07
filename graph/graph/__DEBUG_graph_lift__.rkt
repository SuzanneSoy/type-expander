#lang typed/racket

(define-syntax (l stx)
  #`(let ()
      #,(syntax-local-lift-expression #'(begin (define-type Foo (List 'foo Foo Bar One)) (void)))
      #,(syntax-local-lift-expression #'(begin (define-type Bar (List 'foo Foo Bar One)) (void)))
      1))

(l)