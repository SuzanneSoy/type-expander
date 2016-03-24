#lang typed/racket

(module test typed/racket
  (require (for-syntax (submod "graph.lp2.rkt" test-syntax)
                       syntax/strip-context))
  (provide g gr gr-simple)
  
  (define-syntax (insert-tests stx)
    (replace-context stx tests))
  
  (require "graph.lp2.rkt"
           (only-in "../lib/low.rkt" cars cdrs check-equal?: check-true: % in)
           (only-in "adt.lp2.rkt" uniform-get)
           "../type-expander/type-expander.lp2.rkt")
  
  (insert-tests)
  
  (define counter : Integer 0)
  (define-graph gr-simple
    [Fountain [ctr : Integer]
              [water : (Listof Symbol)]
              [(m-fountain [mountain : Symbol])
               (set! counter (+ counter 1))
               (Fountain counter (list mountain mountain))]]
    [Node2 [sym : Symbol]
           [(m-node2 [s : Symbol])
            (Node2 s)]]
    [Node3 [err : Nothing]
           [(m-node3)
            (error "Should never be called")]])

  ;; Check that the two requests for (splash) give the same node:
  ;; Also, (n2) is disconnected from the rest of the graph.
  (check-true:
   (% ((a b c d) (e) ())
      = (gr-simple #:roots
                   [Fountain '((splash) (splish) (splash) (soak))]
                   [Node2 '((n2))]
                   [Node3 '()])
      in
      (and (= (uniform-get a ctr) (uniform-get c ctr))
           (not (= (uniform-get a ctr) (uniform-get b ctr)))))))