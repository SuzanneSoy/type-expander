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
         (for-syntax syntax/parse)
         (for-syntax syntax/parse/experimental/template))

(require "__DEBUG_graph6B.rkt")

(frozen (~>))

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

#;(define-syntax (blah stx)
  #'(begin
      (define-graph
        first-step
        #:definitions
        ((define-type-expander
           (~> stx)
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
             ((_ (~datum m-streets)) #'(U m-streets4/node (Listof Street))))))
        (City
         (streets : (Let (~> first-step-expander2) (~> m-streets)))
         ((City1/simple-mapping (streets : (~> m-streets))) (City streets)))
        (Street
         (sname : (Let (~> first-step-expander2) String))
         ((Street2/simple-mapping (sname : String)) (Street sname)))
        (m-cities3/node
         (returned : (Listof City))
         ((m-cities (cnames : (Listof (Listof String))))
          (m-cities3/node
           (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
             (define (strings→city (s : (Listof String))) (City (m-streets s)))
             (map strings→city cnames)))))
        (m-streets4/node
         (returned : (Listof Street))
         ((m-streets (snames : (Listof String)))
          (m-streets4/node
           (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
             (map Street snames))))))))

;(blah)