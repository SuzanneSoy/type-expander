#lang typed/racket

(module test-~>-bound typed/racket
  (require "graph-6-rich-returns.lp2.rkt"
           phc-toolkit
           "get.lp2.rkt"
           "../type-expander/type-expander.lp2.rkt")
  
  (define-type blob String)
  (define-type-expander (bubble stx) #'String)
  
  (define-graph
    grr3
    ([City [streets : (~> m-streets)]]
     [Street [sname : String]])
    [(m-cities [cnames : (Listof (Listof bubble))])
     : (Listof City)
     (define (strings→city [s : (Listof blob)])
       (City (m-streets s)))
     (map strings→city cnames)]
    [(m-streets [snames : (Listof String)])
     : (Listof Street)
     (map Street snames)])
  
  (check-equal?: (% (x y) = (grr3 '(("a" "b" "c") ("d")))
                    in
                    (list (get x streets … sname)
                          (get y streets … sname)))
                 '(("a" "b" "c") ("d")))
  
  ;; Check that there are no collisions:
  ;; Same as above with just the graph name changed
  (define-graph
    grr4
    ([City [streets : (~> m-streets)]]
     [Street [sname : String]])
    [(m-cities [cnames : (Listof (Listof bubble))])
     : (Listof City)
     (define (strings→city [s : (Listof blob)])
       (City (m-streets s)))
     (map strings→city cnames)]
    [(m-streets [snames : (Listof String)])
     : (Listof Street)
     (map Street snames)]))

(module test-~>-unbound typed/racket
  (require "graph-6-rich-returns.lp2.rkt"
           (only-in phc-toolkit check-equal?: …)
           "get.lp2.rkt"
           "../type-expander/type-expander.lp2.rkt")
  
  (define-type blob String)
  (define-type-expander (bubble stx) #'String)
  
  (define-graph
    grr3
    ([City [streets : (~> m-streets)]]
     [Street [sname : String]])
    [(m-cities [cnames : (Listof (Listof bubble))])
     : (Listof City)
     (define (strings→city [s : (Listof blob)])
       (City (m-streets s)))
     (map strings→city cnames)]
    [(m-streets [snames : (Listof String)])
     : (Listof Street)
     (map Street snames)])
  
  (check-equal?: (let ([l (grr3 '(("a" "b" "c") ("d")))])
                   (list (get (car l) streets … sname)
                         (get (cadr l) streets … sname)))
                 '(("a" "b" "c") ("d")))
  
  ;; Check that there are no collisions:
  ;; Same as above with just the graph name changed
  (define-graph
    grr4
    ([City [streets : (~> m-streets)]]
     [Street [sname : String]])
    [(m-cities [cnames : (Listof (Listof bubble))])
     : (Listof City)
     (define (strings→city [s : (Listof blob)])
       (City (m-streets s)))
     (map strings→city cnames)]
    [(m-streets [snames : (Listof String)])
     : (Listof Street)
     (map Street snames)]))

(module test typed/racket
  (require (submod ".." test-~>-bound))
  (require (submod ".." test-~>-unbound)))