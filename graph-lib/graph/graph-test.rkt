#lang typed/racket

(module test typed/racket
  (require (for-syntax (submod "graph.lp2.rkt" test-syntax)
                       syntax/strip-context))
  
  (provide g gr gr-simple)
  
  (define-syntax (insert-tests stx)
    (replace-context stx tests))
  
  (require "graph.lp2.rkt"
           (only-in "../lib/low.rkt" cars cdrs check-equal?:)
           (only-in "adt.lp2.rkt" uniform-get)
           "../type-expander/type-expander.lp2.rkt")
  
  (insert-tests)

  (define-graph gr-simple
    [Fountain [water : (Listof Symbol)]
              [(m-fountain [mountain : Symbol])
               (Fountain (list mountain mountain))]]))