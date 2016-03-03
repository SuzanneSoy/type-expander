#lang typed/racket

(require "graph-6-rich-returns.lp2.rkt"
         "../lib/low.rkt"
         "graph.lp2.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         "../type-expander/multi-id.lp2.rkt"
         "structure.lp2.rkt" ; debug
         "variant.lp2.rkt" ; debug
         "fold-queues.lp2.rkt"; debug
         "rewrite-type.lp2.rkt"; debug
         "meta-struct.rkt"; debug
         racket/splicing; debug
         (for-syntax syntax/parse)
         (for-syntax syntax/parse/experimental/template))

#|
(require "__DEBUG_graph6B.rkt")

(frozen (~>))
|#


(define-type blob String)
(define-type-expander (bubble stx) #'String)

(require (for-syntax syntax/strip-context))

(define-syntax (super-define-graph/rich-return stx)
  (syntax-case stx ()
    [(_ name . rest)
     (with-syntax ([(b (d (dgi n) . r) (dgi2 n2))
                    (replace-context
                     stx
                     #'(begin
                         (define-syntax-rule (dg1 name)
                           (define-graph/rich-return name ~> . rest))
                         (dg1 name)))])
       #'(b (d (dgX n) . r) (dgX n2)))]))

(super-define-graph/rich-return
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

#;(super-define-graph/rich-return
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

#|

(define-syntax-rule (dg grr)
  (define-graph/rich-return grr ~>
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

(dg grr)
(dg grra)
|#