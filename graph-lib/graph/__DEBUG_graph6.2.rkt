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

(define-rename-transformer-parameter ~>aaa
  (make-rename-transformer #'+))

(define-graph gr
  #:wrapping-definitions
  (begin
    (define-type-expander
      (first-step-expander1 stx)
      (syntax-parse
          stx
        ((_ (~datum m-cities))
         (template
          (U
           (first-step #:placeholder m-cities3/node)
           (Listof (first-step #:placeholder City)))))
        ((_ (~datum m-streets))
         (template
          (U
           (first-step #:placeholder m-streets4/node)
           (Listof (first-step #:placeholder Street)))))))
    (define-type-expander
      (first-step-expander2 stx)
      (syntax-parse
          stx
        ((_ (~datum m-cities)) #'(U m-cities3/node (Listof City)))
        ((_ (~datum m-streets)) #'(U m-streets4/node (Listof Street)))))
    ;(define-syntax (g-rest stx) (syntax-local-introduce #'))
    (splicing-let-syntaxes
        ()
      (define-graph-rest))
    #;(splicing-syntax-parameterize
        ();([~>aaa (make-rename-transformer #'first-step-expander1)])
      (g-rest))
    #;(define-graph-rest))
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