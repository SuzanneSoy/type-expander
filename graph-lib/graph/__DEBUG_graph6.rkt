#lang typed/racket

(require "graph-6-rich-returns.lp2.rkt"
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

#|
(require "__DEBUG_graph6B.rkt")

(frozen (~>))
|#

















(define-rename-transformer-parameter ~>
  (make-rename-transformer #'+))



(begin
  (define-graph
    first-step
    #:wrapping-definitions
    (begin
      (define-type-expander
        (first-step-expander1 stx)
        #;#'Number
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
        #;#'Number
        (syntax-parse
              stx
            ((_ (~datum m-cities)) #'(U m-cities3/node (Listof City)))
            ((_ (~datum m-streets)) #'(U m-streets4/node (Listof Street)))))
      (splicing-let-syntax
          ([~> (make-rename-transformer #'first-step-expander1)])
        (define-graph-rest))
      #;(splicing-syntax-parameterize
          ((~> (make-rename-transformer #'first-step-expander1)))
        (define-graph-rest)))
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
         (map Street snames)))))))






















#|
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


;(first-step '(("a" "b") ("c" "d")))







#;(begin
  (define-multi-id
   first-step
   #:type-expander
   (λ (stx)
     (syntax-parse
      stx
      ((_ (~datum City)) #'City45/with-promises-type)
      ((_ (~datum Street)) #'Street46/with-promises-type)
      ((_ (~datum m-cities3/node)) #'m-cities3/node47/with-promises-type)
      ((_ (~datum m-streets4/node)) #'m-streets4/node48/with-promises-type)
      ((_ #:incomplete (~datum City)) #'City25/incomplete-type)
      ((_ #:incomplete (~datum Street)) #'Street26/incomplete-type)
      ((_ #:incomplete (~datum m-cities3/node))
       #'m-cities3/node27/incomplete-type)
      ((_ #:incomplete (~datum m-streets4/node))
       #'m-streets4/node28/incomplete-type)
      ((_ #:make-incomplete (~datum City))
       #'(→ streets41/incomplete-type City25/incomplete-type))
      ((_ #:make-incomplete (~datum Street))
       #'(→ sname42/incomplete-type Street26/incomplete-type))
      ((_ #:make-incomplete (~datum m-cities3/node))
       #'(→ returned43/incomplete-type m-cities3/node27/incomplete-type))
      ((_ #:make-incomplete (~datum m-streets4/node))
       #'(→ returned44/incomplete-type m-streets4/node28/incomplete-type))
      ((_ #:incomplete (~datum City) fld)
       (syntax-parse #'fld ((~datum streets) #'streets41/incomplete-type)))
      ((_ #:incomplete (~datum Street) fld)
       (syntax-parse #'fld ((~datum sname) #'sname42/incomplete-type)))
      ((_ #:incomplete (~datum m-cities3/node) fld)
       (syntax-parse #'fld ((~datum returned) #'returned43/incomplete-type)))
      ((_ #:incomplete (~datum m-streets4/node) fld)
       (syntax-parse #'fld ((~datum returned) #'returned44/incomplete-type)))
      ((_ #:make-placeholder (~datum City))
       #'(→ (~> m-streets) City21/placeholder-type))
      ((_ #:make-placeholder (~datum Street))
       #'(→ String Street22/placeholder-type))
      ((_ #:make-placeholder (~datum m-cities3/node))
       #'(→ (Listof (Listof String)) m-cities3/node23/placeholder-type))
      ((_ #:make-placeholder (~datum m-streets4/node))
       #'(→ (Listof String) m-streets4/node24/placeholder-type))
      ((_ #:placeholder (~datum City)) #'City21/placeholder-type)
      ((_ #:placeholder (~datum Street)) #'Street22/placeholder-type)
      ((_ #:placeholder (~datum m-cities3/node))
       #'m-cities3/node23/placeholder-type)
      ((_ #:placeholder (~datum m-streets4/node))
       #'m-streets4/node24/placeholder-type)))
   #:call
   (λ (stx)
     (syntax-parse
      stx
      ((_ #:λroot (~datum City)) #'City5/constructor)
      ((_ #:λroot (~datum Street)) #'Street6/constructor)
      ((_ #:λroot (~datum m-cities3/node)) #'m-cities3/node7/constructor)
      ((_ #:λroot (~datum m-streets4/node)) #'m-streets4/node8/constructor)
      ((_ #:root (~datum City) . rest)
       (syntax/loc stx (City5/constructor . rest)))
      ((_ #:root (~datum Street) . rest)
       (syntax/loc stx (Street6/constructor . rest)))
      ((_ #:root (~datum m-cities3/node) . rest)
       (syntax/loc stx (m-cities3/node7/constructor . rest)))
      ((_ #:root (~datum m-streets4/node) . rest)
       (syntax/loc stx (m-streets4/node8/constructor . rest)))
      ((_ . rest) (syntax/loc stx (City5/constructor . rest)))))
   #:id
   (λ (stx) #'City5/constructor))
  (begin
    (: City9/make-placeholder City13/make-placeholder-type)
    (define (City9/make-placeholder streets)
      (City17/placeholder-struct (list streets))))
  (begin
    (: Street10/make-placeholder Street14/make-placeholder-type)
    (define (Street10/make-placeholder sname)
      (Street18/placeholder-struct (list sname))))
  (begin
    (:
     m-cities3/node11/make-placeholder
     m-cities3/node15/make-placeholder-type)
    (define (m-cities3/node11/make-placeholder cnames)
      (m-cities3/node19/placeholder-struct (list cnames))))
  (begin
    (:
     m-streets4/node12/make-placeholder
     m-streets4/node16/make-placeholder-type)
    (define (m-streets4/node12/make-placeholder snames)
      (m-streets4/node20/placeholder-struct (list snames))))
  (begin
    (: City29/make-incomplete City33/make-incomplete-type)
    (define (City29/make-incomplete streets)
      (list 'City37/incomplete-tag streets)))
  (begin
    (: Street30/make-incomplete Street34/make-incomplete-type)
    (define (Street30/make-incomplete sname)
      (list 'Street38/incomplete-tag sname)))
  (begin
    (: m-cities3/node31/make-incomplete m-cities3/node35/make-incomplete-type)
    (define (m-cities3/node31/make-incomplete returned)
      (list 'm-cities3/node39/incomplete-tag returned)))
  (begin
    (:
     m-streets4/node32/make-incomplete
     m-streets4/node36/make-incomplete-type)
    (define (m-streets4/node32/make-incomplete returned)
      (list 'm-streets4/node40/incomplete-tag returned)))
  (begin (struct (A) City17/placeholder-struct ((f : A)) #:transparent))
  (begin (struct (A) Street18/placeholder-struct ((f : A)) #:transparent))
  (begin
    (struct (A) m-cities3/node19/placeholder-struct ((f : A)) #:transparent))
  (begin
    (struct (A) m-streets4/node20/placeholder-struct ((f : A)) #:transparent))
  (begin (struct City49/index-type ((i : Index)) #:transparent))
  (begin (struct Street50/index-type ((i : Index)) #:transparent))
  (begin (struct m-cities3/node51/index-type ((i : Index)) #:transparent))
  (begin (struct m-streets4/node52/index-type ((i : Index)) #:transparent))
  (splicing-let
   ((City1/simple-mapping City9/make-placeholder)
    (Street2/simple-mapping Street10/make-placeholder)
    (m-cities m-cities3/node11/make-placeholder)
    (m-streets m-streets4/node12/make-placeholder)
    (City City29/make-incomplete)
    (Street Street30/make-incomplete)
    (m-cities3/node m-cities3/node31/make-incomplete)
    (m-streets4/node m-streets4/node32/make-incomplete))
   (begin
     (define-type-expander
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
      (displayln (format "first-step-expander2: ~a" stx))
      (syntax-parse
       stx
       ((_ (~datum m-cities)) #'(U m-cities3/node (Listof City)))
       ((_ (~datum m-streets)) #'(U m-streets4/node (Listof Street))))))
   (define-graph-second-step
    ((City5/constructor
      Street6/constructor
      m-cities3/node7/constructor
      m-streets4/node8/constructor)
     City5/constructor
     (City9/make-placeholder
      Street10/make-placeholder
      m-cities3/node11/make-placeholder
      m-streets4/node12/make-placeholder)
     (City13/make-placeholder-type
      Street14/make-placeholder-type
      m-cities3/node15/make-placeholder-type
      m-streets4/node16/make-placeholder-type)
     (City17/placeholder-struct
      Street18/placeholder-struct
      m-cities3/node19/placeholder-struct
      m-streets4/node20/placeholder-struct)
     (City21/placeholder-type
      Street22/placeholder-type
      m-cities3/node23/placeholder-type
      m-streets4/node24/placeholder-type)
     (City25/incomplete-type
      Street26/incomplete-type
      m-cities3/node27/incomplete-type
      m-streets4/node28/incomplete-type)
     (City29/make-incomplete
      Street30/make-incomplete
      m-cities3/node31/make-incomplete
      m-streets4/node32/make-incomplete)
     (City33/make-incomplete-type
      Street34/make-incomplete-type
      m-cities3/node35/make-incomplete-type
      m-streets4/node36/make-incomplete-type)
     (City37/incomplete-tag
      Street38/incomplete-tag
      m-cities3/node39/incomplete-tag
      m-streets4/node40/incomplete-tag)
     ((streets41/incomplete-type)
      (sname42/incomplete-type)
      (returned43/incomplete-type)
      (returned44/incomplete-type))
     (City45/with-promises-type
      Street46/with-promises-type
      m-cities3/node47/with-promises-type
      m-streets4/node48/with-promises-type)
     City45/with-promises-type
     (City49/index-type
      Street50/index-type
      m-cities3/node51/index-type
      m-streets4/node52/index-type))
    (first-step
     #:debug
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
       (displayln (format "first-step-expander2: ~a" stx))
       (syntax-parse
        stx
        ((_ (~datum m-cities)) #'(U m-cities3/node (Listof City)))
        ((_ (~datum m-streets)) #'(U m-streets4/node (Listof Street))))))
     (City
      (streets
       :
       (Let (~> first-step-expander2) (U (Pairof '~> (U)) (~> m-streets))))
      ((City1/simple-mapping (streets : (~> m-streets))) (City streets)))
     (Street
      (sname : (Let (~> first-step-expander2) (U (Pairof '~> (U)) String)))
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
          (map Street snames)))))))))








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
|#