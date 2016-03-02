#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide (all-from-out racket/match)
           ∘
           …
           …+
           match-λ
           match-λ*
           match-λ**
           generate-temporary)
  
  (require racket/match)
  
  (require (only-in racket
                    [compose ∘]
                    [... …])
           (only-in syntax/parse
                    [...+ …+]))
  
  (require (only-in racket/match
                    [match-lambda match-λ]
                    [match-lambda* match-λ*]
                    [match-lambda** match-λ**]))
  
  (require/typed racket/syntax [generate-temporary (→ Any Identifier)]))
