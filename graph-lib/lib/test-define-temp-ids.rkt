#lang racket
(require "low-untyped.rkt")

(with-syntax ([((foo ...) ...) #'((aa bb cc) (x1 x2))])
  (define-temp-ids "___~a.truc" ((foo ...) ...) #:first-base fst)
  (syntax->datum #'((___foo.truc ...) ...))
  (syntax->datum #'(fst ___fst.truc))
  (void))

(with-syntax ([(foo ...) #'(aa bb cc)])
  (define-temp-ids "___~a.truc" (foo ...) #:first-base fst)
  (syntax->datum #'(___foo.truc ...))
  (syntax->datum #'(fst ___fst.truc))
  (void))

(with-syntax ([foo #'aa])
  (define-temp-ids "___~a.truc" foo)
  (syntax->datum #'___foo.truc)
  (syntax->datum #'(fst ___fst.truc))
  (void))

(with-syntax ([((foo ...) ...) #'((aa bb cc) (x1 x2))])
  (define-temp-ids "___~a.truc" ((foo ...) ...) #:first-base fst)
  (syntax->datum #'(___foo.truc ... ...))
  (syntax->datum #'(fst ___fst.truc))
  (void))
