#lang typed/racket

(require "graph-6-rich-returns.lp2.rkt"
         "../lib/low.rkt"
         "graph.lp2.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         "../type-expander/multi-id.lp2.rkt"
         "adt.lp2.rkt" ; debug
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


(require "../lib/debug-syntax.rkt")

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

(require (for-syntax racket/format
                     "rewrite-type.lp2.rkt"
                     racket/syntax
                     syntax/parse
                     (submod "../lib/low.rkt" untyped))
         (for-syntax syntax/parse
                     syntax/parse/experimental/template
                     racket/syntax
                     (submod "../lib/low.rkt" untyped)
                     "rewrite-type.lp2.rkt" #|debug|#
                     syntax/id-set
                     racket/format
                     mischief/transform)
         (rename-in "../lib/low.rkt" [~> threading:~>])
         "graph.lp2.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt"
         "../type-expander/multi-id.lp2.rkt"
         "adt.lp2.rkt" ; debug
         "fold-queues.lp2.rkt"; debug
         "rewrite-type.lp2.rkt"; debug
         "meta-struct.rkt"; debug
         racket/stxparam
         racket/splicing)
(begin
  (define-graph
    grr31/first-step
    #:definitions
    ((define-type-expander
       (~> stx)
       (syntax-parse
           stx
         ((_ (~datum m-cities)) (template (U (grr31/first-step #:placeholder m-cities4/node) (Listof (grr31/first-step #:placeholder City)))))
         ((_ (~datum m-streets)) (template (U (grr31/first-step #:placeholder m-streets5/node) (Listof (grr31/first-step #:placeholder Street)))))))
     (define-type-expander (first-step-expander2 stx) (syntax-parse stx ((_ (~datum m-cities)) #'(U m-cities4/node (Listof City))) ((_ (~datum m-streets)) #'(U m-streets5/node (Listof Street))))))
    (City (streets : (Let (~> first-step-expander2) (~> m-streets))) ((City2/simple-mapping (streets : (~> m-streets))) (City streets)))
    (Street (sname : (Let (~> first-step-expander2) String)) ((Street3/simple-mapping (sname : String)) (Street sname)))
    (m-cities4/node
     (returned : (Listof City))
     ((m-cities (cnames : (Listof (Listof bubble))))
      (m-cities4/node (let ((City City2/simple-mapping) (Street Street3/simple-mapping)) (define (strings→city (s : (Listof blob))) (City (m-streets s))) (map strings→city cnames)))))
    (m-streets5/node (returned : (Listof Street)) ((m-streets (snames : (Listof String))) (m-streets5/node (let ((City City2/simple-mapping) (Street Street3/simple-mapping)) (map Street snames))))))
  
  (define-graph
    grr3
    #:definitions
    ((define-type-expander (~>-to-result-type stx) (syntax-parse stx ((_ (~datum m-cities)) #'(Listof City)) ((_ (~datum m-streets)) #'(Listof Street))))
     (define-type m-cities10/node-marker (U (grr31/first-step m-cities4/node) (Listof (grr31/first-step City))))
     (define-type m-streets11/node-marker (U (grr31/first-step m-streets5/node) (Listof (grr31/first-step Street))))
     (define-type-expander (second-step-marker-expander stx) (syntax-parse stx ((_ (~datum m-cities)) #'m-cities10/node-marker) ((_ (~datum m-streets)) #'m-streets11/node-marker)))
     (define-type second-step-m-cities16/node-of-first (grr31/first-step m-cities4/node))
     (define-type second-step-m-streets17/node-of-first (grr31/first-step m-streets5/node))
     (define-type-expander
       (second-step-marker2-expander stx)
       (syntax-parse
           stx
         ((_ (~datum m-cities)) #'(U second-step-m-cities16/node-of-first (Listof (grr31/first-step City))))
         ((_ (~datum m-streets)) #'(U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street))))))
     #;(define-type-expander
         (inline-type stx)
         (dbg
          ("inline-type" stx)
          (syntax-parse
              stx
            ((_ i-t (~and seen (:id …)))
             (let ((seen-list (syntax->list #'seen)))
               (when (and (not (null? seen-list)) (member (car seen-list) (cdr seen-list) free-identifier=?))
                 (raise-syntax-error
                  'define-graph/rich-returns
                  (~a "Cycles in types are not allowed." " The following types were already inlined: " (syntax->datum #'seen) ", but " #'t " appeared a second time.")
                  #'t)))
             (replace-in-type
              #'(Let (~> second-step-marker-expander) i-t)
              #'((m-cities10/node-marker (inline-type (Listof City) (m-cities4/node . seen)))
                 (m-streets11/node-marker (inline-type (Listof Street) (m-streets5/node . seen)))
                 (City (grr3 #:placeholder City))
                 (Street (grr3 #:placeholder Street))))))))
     (define-syntax (inline-instance stx)
       (dbg
        ("inline-instance" stx)
        (syntax-parse
            stx
          ((_ i-t (~and seen (:id …)))
           (define/with-syntax typp #'(Let (~> second-step-marker2-expander) i-t))
           (define/with-syntax
             repl
             (replace-in-instance
              #'typp
              #'((second-step-m-cities16/node-of-first Symbol (grr31/first-step #:? m-cities4/node) (λ _ (error "NIY4")))
                 (second-step-m-streets17/node-of-first Symbol (grr31/first-step #:? m-streets5/node) (λ _ (error "NIY4")))
                 (City (grr3 #:placeholder City) (grr31/first-step #:? City) (λ _ (error "NIY3")))
                 (Street (grr3 #:placeholder Street) (grr31/first-step #:? Street) (λ _ (error "NIY3"))))))
           (displayln (list "i-t=" #'typp))
           (let ((seen-list (syntax->list #'seen)))
             (when (and (not (null? seen-list)) (member (car seen-list) (cdr seen-list) free-identifier=?))
               (raise-syntax-error
                'define-graph/rich-returns
                (~a "Cycles in types are not allowed." " The following types were already inlined: " (syntax->datum #'seen) ", but " #'t " appeared a second time.")
                #'t)))
           #'(λ ((x : (Let (~> second-step-marker2-expander) i-t))) repl (error "NIY2")))))))
    (City (streets : (Let (~> ~>-to-result-type) (~> m-streets)))
          ((City6/extract/mapping (from : (grr31/first-step City)))
           (City ((inline-instance (~> m-streets) ())
                  (get from streets)))))
    (Street (sname : (Let (~> ~>-to-result-type) String))
            ((Street7/extract/mapping (from : (grr31/first-step Street)))
             (Street ((inline-instance String ())
                      (get from sname)))))))