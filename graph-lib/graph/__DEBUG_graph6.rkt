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

;(grr3 '(("a" "b") ("c")))

#;(super-define-graph/rich-return
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










;; DEBUG:
#;(require (for-syntax racket/format
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




#;(begin
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
   (define-type second-step-City18-of-first (grr31/first-step City))
   (define-type second-step-Street19-of-first (grr31/first-step Street))
   (define-type-expander
     (second-step-marker2-expander stx)
     (syntax-parse
         stx
       ((_ (~datum m-cities)) #'(U second-step-m-cities16/node-of-first (Listof (grr31/first-step City))))
       ((_ (~datum m-streets)) #'(U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street))))))
   (define-type-expander
     (inline-type* stx)
     (dbg
      ("inline-type" stx)
      (syntax-parse
          stx
        ((_ i-tyy (~and seen (:id …)))
         (define/with-syntax replt (replace-in-type #'(Let (~> second-step-marker2-expander) i-tyy) #'((City second-step-City18-of-first) (Street second-step-Street19-of-first))))
         #'(inline-type replt seen)))))
   (define-type-expander
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
          #'(Let ((~> second-step-marker-expander)) i-t)
          #'((m-cities10/node-marker (inline-type (Listof City) (m-cities4/node . seen)))
             (m-streets11/node-marker (inline-type (Listof Street) (m-streets5/node . seen)))
             (second-step-City18-of-first (grr3 #:placeholder City))
             (second-step-Street19-of-first (grr3 #:placeholder Street))))))))
   (define-syntax (inline-instance* stx)
     (dbg
      ("inline-instance*" stx)
      (syntax-parse
          stx
        ((_ i-ty seen)
         (define/with-syntax replt (replace-in-type #'(Let (~> second-step-marker2-expander) i-ty) #'((City second-step-City18-of-first) (Street second-step-Street19-of-first))))
         (displayln (list "replt=" #'replt))
         #'(inline-instance replt seen)))))
   (define-syntax (inline-instance stx)
     (dbg
      ("inline-instance" stx)
      (syntax-parse
          stx
        ((_ i-t (~and seen (:id …)))
         (define/with-syntax typp #'i-t)
         (define/with-syntax
           repl
           (replace-in-instance
            #'typp
            #'((second-step-m-cities16/node-of-first (inline-type* (Listof City) (m-cities4/node . seen)) (grr31/first-step #:? m-cities4/node) (inline-instance* (Listof City) (m-cities4/node . seen)))
               (second-step-m-streets17/node-of-first (inline-type* (Listof Street) (m-streets5/node . seen)) (grr31/first-step #:? m-streets5/node) (inline-instance* (Listof Street) (m-streets5/node . seen)))
               (second-step-City18-of-first (grr3 #:placeholder City) (grr31/first-step #:? City) City6/extract/mapping)
               (second-step-Street19-of-first (grr3 #:placeholder Street) (grr31/first-step #:? Street) Street7/extract/mapping))))
         (displayln (list "i-t=" #'typp))
         (let ((seen-list (syntax->list #'seen)))
           (when (and (not (null? seen-list)) (member (car seen-list) (cdr seen-list) free-identifier=?))
             (raise-syntax-error
              'define-graph/rich-returns
              (~a "Cycles in types are not allowed." " The following types were already inlined: " (syntax->datum #'seen) ", but " #'t " appeared a second time.")
              #'t)))
         #'(λ ((x : i-t)) repl (error "NIY2")))))))
  (City (streets : (Let (~> ~>-to-result-type) (~> m-streets)))
        ((City6/extract/mapping (from : (grr31/first-step City)))
         (City
          ;((inline-instance* (~> m-streets) ()) (get from streets))
          #;((inline-instance
             (U
              second-step-m-streets17/node-of-first
              (Listof
               grr31/first-step:Street2/promise-type))
             ())
             (get from streets))
          ((λ ((x : (U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street)))))
            (λ ((val : (U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street)))))
              (first-value
               ((λ ((val : (U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street)))) (acc : Void))
                  :
                  (values (U (inline-type* (Listof Street) (m-streets5/node)) (Listof (grr31/first-step Street))) Void)
                  (cond
                    (((grr31/first-step #:? m-streets5/node) val)
                     ((ann
                       (λ ((x : second-step-m-streets17/node-of-first) (acc : Void)) (values ((inline-instance* (Listof Street) (m-streets5/node)) (get x returned)) acc))
                       (→ second-step-m-streets17/node-of-first Void (values (inline-type* (Listof Street) (m-streets5/node)) Void)))
                      val
                      acc))
                    (#t
                     ((λ ((val : (Listof (grr31/first-step Street))) (acc : Void))
                        :
                        (values (Listof (grr31/first-step Street)) Void)
                        (let ((f
                               ((inst foldl (grr31/first-step Street) (Pairof (Listof (grr31/first-step Street)) Void) Nothing Nothing)
                                (λ ((x : (grr31/first-step Street)) (acc1 : (Pairof (Listof (grr31/first-step Street)) Void)))
                                  (let-values (((res res-acc) ((inst values (grr31/first-step Street) Void) x (cdr acc1)))) (cons (cons res (car acc1)) res-acc)))
                                (cons '() acc)
                                val)))
                          (values (reverse (car f)) (cdr f))))
                      val
                      acc))
                    (else
                     (typecheck-fail
                      (U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street)))
                      "Unhandled union case in (U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street))),"
                      #;" whole type was:(U second-step-m-streets17/node-of-first (Listof (grr31/first-step Street)))"))))
                val
                (void))))
            (error "NIY2"))
           (get from streets))
          )))
  (Street (sname : (Let (~> ~>-to-result-type) String)) ((Street7/extract/mapping (from : (grr31/first-step Street))) (Street ((inline-instance* String ()) (get from sname))))))

)