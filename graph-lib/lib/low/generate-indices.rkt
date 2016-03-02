#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide generate-indices)
  
  (require "typed-untyped.rkt")
  (require-typed/untyped "sequence.rkt")
  (: generate-indices (∀ (T) (case→ (→ Integer (Syntax-Listof T)
                                       (Listof Integer))
                                    (→ (Syntax-Listof T)
                                       (Listof Nonnegative-Integer)))))
  
  (define generate-indices
    (case-lambda
      [(start stx)
       (for/list ([v (my-in-syntax stx)]
                  [i (in-naturals start)])
         i)]
      [(stx)
       (for/list ([v (my-in-syntax stx)]
                  [i : Nonnegative-Integer
                     (ann (in-naturals) (Sequenceof Nonnegative-Integer))])
         i)])))