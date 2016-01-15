#lang typed/racket

;; Using check-equal? on our variants result in the following error message:
;; Attempted to use a higher-order value passed as `Any` in untyped code
;; check-equal? and check-not-equal? are replaced by versions that work with
;; “higher-order values” below.

(require (except-in (only-meta-in 0 typed/rackunit)
                    ;; Above: typed/racket risks complaining that it can't do
                    ;; for-meta in all-from-out if we don't use `only-meta-in`.
                    check-equal?
                    check-not-equal?))

(provide (all-from-out typed/rackunit)
         check-equal?
         check-not-equal?
         check-eval-equal?
         check-eval-string-equal?
         check-eval-string-equal?/ns)

(require "eval-get-values.rkt")

(require syntax/parse/define)

(define-simple-macro (check-equal? x y . message)
  (check-true (equal? x y) . message))

(define-simple-macro (check-not-equal? x y . message)
  (check-true (not (equal? x y)) . message))

(define-simple-macro (check-eval-equal? to-eval y . message)
  (check-true (equal? (eval-get-values to-eval
                                       (variable-reference->namespace
                                        (#%variable-reference)))
                      y)
              . message))

(define-simple-macro (check-eval-string-equal? to-eval y . message)
  (check-true (equal? (eval-get-values (read (open-input-string to-eval))
                                       (variable-reference->namespace
                                        (#%variable-reference)))
                      y)
              . message))

(define-simple-macro (check-eval-string-equal?/ns ns-anchor to-eval y . message)
  (check-true (equal? (eval-get-values (read (open-input-string to-eval))
                                       (namespace-anchor->namespace
                                        ns-anchor))
                      y)
              . message))

(define-syntax-rule (test-module body ...)
  (module* test typed/racket
    (require (submod ".."))
    body ...))