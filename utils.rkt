#lang typed/racket

(require (for-syntax syntax/parse))

(provide define-syntax/parse)

;; Copied from phc-toolkit, but does not bind "stx". Use this-syntax
;; instead, from syntax/parse.
(define-syntax-rule (define-syntax/parse (name . args) body0 . body)
  (define-syntax (name stx2)
    ;(with-backtrace (syntax->datum stx2)
    (syntax-parse stx2
      [(_ . args) body0 . body])))