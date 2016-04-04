#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide set-map→set)
  (: set-map→set (∀ (e b) (→ (Setof e) (→ e b) (Setof b))))
  (define (set-map→set s f) (list->set (set-map s f))))