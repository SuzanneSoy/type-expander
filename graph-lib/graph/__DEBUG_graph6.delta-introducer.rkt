#lang typed/racket

(require "graph-6-rich-returns.delta-introducer.lp2.rkt"
         (except-in "../lib/low.rkt" ~>)
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
         racket/stxparam; debug
         (for-syntax syntax/parse)
         (for-syntax syntax/parse/experimental/template))

(define-graph/rich-return grr
  ([City [streets : (~> m-streets)]]
   [Street [sname : String]])
  [(m-cities [cnames : (Listof (Listof String))])
   : (Listof City)
   (define (strings→city [s : (Listof String)])
     (City (m-streets s)))
   (map strings→city cnames)]
  [(m-streets [snames : (Listof String)])
   : (Listof Street)
   (map Street snames)])

#;(define-graph/rich-return grra
  ([City [streets : (~> m-streets)]]
   [Street [sname : String]])
  [(m-cities [cnames : (Listof (Listof String))])
   : (Listof City)
   (define (strings→city [s : (Listof String)])
     (City (m-streets s)))
   (map strings→city cnames)]
  [(m-streets [snames : (Listof String)])
   : (Listof Street)
   (map Street snames)])

