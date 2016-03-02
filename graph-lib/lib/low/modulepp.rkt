#lang racket

(begin-for-syntax
  (define partially-defined-module++ (make-hash

(define-syntax (module++ stx)
  (syntax-case stx
    [(_ name lang . body)
     (syntax-local-lift-module-end-declaration #'define-module )