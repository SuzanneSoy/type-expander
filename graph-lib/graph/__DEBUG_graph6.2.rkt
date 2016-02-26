#lang typed/racket

(require ;"graph-6-rich-returns.lp2.rkt"
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

(define-graph gr
  #:wrapping-definitions (begin (define-graph-rest))
  [City [streets : (Listof Street)] [people : (Listof Person)]
        [(m-city [c : (Listof (Pairof String String))])
         (City (remove-duplicates (map (curry m-street c) (cdrs c)))
               (remove-duplicates (map m-person (cars c))))]]
  [Street [sname : String] [houses : (Listof House)]
          [(m-street [c : (Listof (Pairof String String))] [s : String])
           (Street s (map (curry (curry m-house s) c)
                          (cars (filter (λ ([x : (Pairof String String)])
                                          (equal? (cdr x) s))
                                        c))))]]
  [House [owner : Person] [location : Street]
         [(m-house [s : String]
                   [c : (Listof (Pairof String String))]
                   [p : String])
          (House (m-person p) (m-street c s))]]
  [Person [name : String]
          [(m-person [p : String])
           (Person p)]])