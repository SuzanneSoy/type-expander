#lang racket
(require "low-untyped.rkt")

(with-syntax ([((foo ...) ...) #'((aa bb cc) (x1 x2))])
  (define-temp-ids "___~a.truc" ((foo ...) ...) #:first-base fst)
  (displayln (syntax->datum #'((___foo.truc ...) ...)))
  (displayln (syntax->datum #'(fst ___fst.truc))))

(newline)

(with-syntax ([(foo ...) #'(aa bb cc)])
  (define-temp-ids "___~a.truc" (foo ...) #:first-base fst)
  (displayln (syntax->datum #'(___foo.truc ...)))
  (displayln (syntax->datum #'(fst ___fst.truc))))

(newline)

(with-syntax ([foo #'aa])
  (define-temp-ids "___~a.truc" foo)
  (displayln (syntax->datum #'___foo.truc))
  (displayln (syntax->datum #'(fst ___fst.truc))))

(newline)

(with-syntax ([((foo ...) ...) #'((aa bb cc) (x1 x2))])
  (define-temp-ids "___~a.truc" ((foo ...) ...) #:first-base fst)
  (displayln (syntax->datum #'(___foo.truc ... ...)))
  (displayln (syntax->datum #'(fst ___fst.truc))))

(newline)

(define a 1)
(+ a a)

(module t typed/racket
  (require "low.rkt"))