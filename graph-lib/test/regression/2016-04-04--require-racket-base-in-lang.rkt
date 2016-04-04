#lang s-exp "../../lib.rkt"

(module test "../../lib.rkt"
 (define-syntax (t stx)
   ;; `+` and other identifiers from racket/base should be available here.
   #`#,(+ 1 2))

  (check-equal?: (t) 3))