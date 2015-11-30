#lang racket
(require "low-untyped.rkt")

(with-syntax ([(foo ...) #'(aa bb cc)])
  (define-temp-ids "___~a.truc" (foo ...) #:first-base fst)
  (displayln (syntax->datum #'(___foo.truc ...)))
  (displayln (syntax->datum #'(fst ___fst.truc))))

(define a 1)
(+ a a)