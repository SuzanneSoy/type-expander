#lang racket
;(require "typed-untyped.rkt")
;(define-typed/untyped-modules #:no-test
(provide show-backtrace
         with-backtrace)

(define backtrace (make-parameter '()))

(define-syntax-rule (with-backtrace push . body)
  (parameterize ([backtrace (cons push (backtrace))])
    . body))

(define (show-backtrace)
  (pretty-write (backtrace)));)