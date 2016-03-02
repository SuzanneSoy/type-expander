#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide ∘ … …+)
  
  (require (only-in racket
                    [compose ∘]
                    [... …])
           (only-in syntax/parse
                    [...+ …+]))
  
  (require racket/match)
  (provide (all-from-out racket/match)
           (rename-out [match-lambda match-λ]
                       [match-lambda* match-λ*]
                       [match-lambda** match-λ**]))

  (require/typed racket/syntax [generate-temporary (→ Any Identifier)]))
