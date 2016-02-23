#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Syntactic sugar for 
 @racket[graph]: rich return types}

@(table-of-contents)

@section{Introduction}

We define a wrapper around the @tc[graph] macro, which
allows defining mappings with rich return types, instead of
being forced to return a single node. For example, a mapping
can return a list of nodes.

During the graph construction, however, the user cannot
access the contents of these rich values. If this was
allowed, constructing a node might cause infinite recursion,
which is precisely one of the pitfalls our library strives
to avoid. For example, the following two constructors each
depend on parts of the other's output.

@chunk[<example-infinite-loop>
       (define-graph (g [a [len : Integer] [bs : (Listof b)]]
                        [b [len : Integer] [as : (Listof a)]])
         [(ma) : (Listof a)
          (let ([bs (mb)])
            (list (a (length bs) bs)
                  (a 42 bs)))]
         [(mb) : (Listof b)
          (let ([as (ma)])
            (list (b (length bs) as)
                  (b 123 as)))])]

In the above example, running @tc[(ma)] will require
running @tc[(mb)] too, to compute the length of the list
returned by @tc[(mb)], and vice-versa. It is clear this code
will run into an infinite loop in an eager language like 
@tc[typed/racket].

To avoid this kind of issue, we will make the mapping
functions return opaque values whose contents cannot be
inspected during the creation of the graph. This also makes
the implementation easier, as we will generate the graph in
two phases: first, we will associate a single-field node
with each mapping, and use it as their return type. Then, a
second pass will break these nodes, and extract their
constituents until an actual user-specified node is
reached.

Since this implementation also allows serveral mappings to
return the same node, the new signature separates the
mapping declarations from the node definitions:

@chunk[<signature>
       (define-graph/rich-return name:id
         ((~commit [node:id <field-signature> …])
          …)
         (~commit <mapping-declaration>)
         …)]

Where @tc[<field-signature>] hasn't changed:

@chunk[<field-signature>
       (~describe "[field : type]"
                  [field:id c:colon field-type:expr])]

We now allow more complex return types in a @tc[<mapping-declaration>]:

@chunk[<mapping-declaration>
       (~describe "[(mapping [param : type] …) : result . body]"
                  [(mapping:id [param:id cp:colon param-type:expr] …)
                   cm:colon result-type:expr
                   . body])]

Here is an example usage of this syntax:

@chunk[<test-graph-rich-return>
       (define-graph/rich-return grr
         ([City [streets : (~> m-streets)]]
          [Street [sname : String]])
         [(m-cities [cnames : (Listof (Listof String))]) : (Listof City)
          (define (strings→city [s : (Listof String)]) : City
            (City (m-streets s)))
          (map strings→city cnames)]
         [(m-streets [snames : (Listof String)]) : (Listof Street)
          (map Street snames)])]

The @tc[(~> m-streets)] type is a special marker which will
be expanded to the return type of @tc[m-streets] (namely 
@tc[(Listof Street)]) in the final graph type. For the first
step, however, it will be expanded to 
@tc[(U (grr #:placeholder m-streets/node) (Listof Street))].
Without this, passing the result of @tc[(m-streets s)] to 
@tc[City] would be impossible: the former is a placeholder
for the temporary node type which encapsulates the result
of @tc[m-streets], while the latter would normally expect a
plain list.

@chunk[<graph-rich-return>
       (define-syntax/parse <signature>
         (define-temp-ids "first-step" name)
         (define-temp-ids "~a/simple-mapping" (node …))
         (define-temp-ids "~a/node" (mapping …))
         (template
          (debug
           (begin
             <first-pass-type-expander>
             (define-graph first-step
               [node [field c field-type] …
                [(node/simple-mapping [field c field-type] …);<first-pass-field-type>] …)
                 (node field …)]] …
               [mapping/node [returned cm result-type]
                [(mapping [param cp param-type] …)
                 (mapping/node
                  (let ([node node/simple-mapping] …)
                    . body))]]
               …)))))]

As explained above, during the first pass, the field types
of nodes will allow placeholders for the temporary nodes
encapsulating the result types of mappings.

@chunk[<first-pass-type-expander>
       (define-type-expander (~> stx)
         (syntax-case stx ()
           [(_ mapping) #'(U mapping/node result-type)] …))]

@; TODO: replace-in-type doesn't work well here, we need to define a
@; type-expander.
@chunk[<first-pass-field-type>
       (tmpl-replace-in-type field-type
                             [(~> mapping) (U mapping/node result-type)] …)]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              syntax/parse/experimental/template
                              racket/syntax
                              syntax/stx
                              "../lib/low-untyped.rkt"
                              "../lib/low/multiassoc-syntax.rkt")
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
                  )
         (provide define-graph/rich-return)
         
         (require (for-syntax racket/pretty))
         (define-syntax (debug stx)
           (syntax-case stx ()
             [(_ body)
              ;; syntax->string
              (pretty-print (syntax->datum #'body))
              #'body]))
         
         
         
         
         
         #;(begin
           (define-type-expander
             (~> stx)
             (syntax-case stx ()
               ((_ m-cities) #'(U m-cities3/node (Listof City)))
               ((_ m-streets) #'(U m-streets4/node (Listof Street)))))
           (define-graph
             first-step #:debug
             (City
              (streets : (U m-streets4/node (Listof Street)))
              ((City1/simple-mapping (streets : (U (first-step #:placeholder m-streets4/node)
                                                   (Listof (first-step #:placeholder Street)))))
               (City streets)))
             (Street
              (sname : String)
              ((Street2/simple-mapping (sname : String)) (Street sname)))
             (m-cities3/node
              (returned : (Listof City))
              ((m-cities (cnames : (Listof (Listof String))))
               (m-cities3/node
                (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
                  (define (strings→city (s : (Listof String)))
                    :
                    (first-step #:placeholder City)
                    (City (m-streets s)))
                  (map strings→city cnames)))))
             (m-streets4/node
              (returned : (Listof Street))
              ((m-streets (snames : (Listof String)))
               (m-streets4/node
                (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
                  (map Street snames)))))))
         
         
         
         



















(begin
  (define-multi-id
   first-step
   #:type-expander
   (λ (stx)
     (syntax-parse
      stx
      ((_ (~datum City)) #'City58/with-promises-type)
      ((_ (~datum Street)) #'Street59/with-promises-type)
      ((_ (~datum m-cities3/node)) #'m-cities3/node60/with-promises-type)
      ((_ (~datum m-streets4/node)) #'m-streets4/node61/with-promises-type)
      ((_ #:incomplete (~datum City)) #'City22/incomplete-type)
      ((_ #:incomplete (~datum Street)) #'Street23/incomplete-type)
      ((_ #:incomplete (~datum m-cities3/node))
       #'m-cities3/node24/incomplete-type)
      ((_ #:incomplete (~datum m-streets4/node))
       #'m-streets4/node25/incomplete-type)
      ((_ #:make-incomplete (~datum City))
       #'(→
          (U
           m-streets4/node17/placeholder-type
           (Listof Street15/placeholder-type))
          City22/incomplete-type))
      ((_ #:make-incomplete (~datum Street))
       #'(→ String Street23/incomplete-type))
      ((_ #:make-incomplete (~datum m-cities3/node))
       #'(→ (Listof City14/placeholder-type) m-cities3/node24/incomplete-type))
      ((_ #:make-incomplete (~datum m-streets4/node))
       #'(→
          (Listof Street15/placeholder-type)
          m-streets4/node25/incomplete-type))
      ((_ #:incomplete (~datum City) fld)
       (syntax-parse
        #'fld
        ((~datum streets)
         #'(U
            m-streets4/node17/placeholder-type
            (Listof Street15/placeholder-type)))))
      ((_ #:incomplete (~datum Street) fld)
       (syntax-parse #'fld ((~datum sname) #'String)))
      ((_ #:incomplete (~datum m-cities3/node) fld)
       (syntax-parse
        #'fld
        ((~datum returned) #'(Listof City14/placeholder-type))))
      ((_ #:incomplete (~datum m-streets4/node) fld)
       (syntax-parse
        #'fld
        ((~datum returned) #'(Listof Street15/placeholder-type))))
      ((_ #:make-placeholder (~datum City))
       #'(→
          (U
           (first-step #:placeholder m-streets4/node)
           (Listof (first-step #:placeholder Street)))
          City14/placeholder-type))
      ((_ #:make-placeholder (~datum Street))
       #'(→ String Street15/placeholder-type))
      ((_ #:make-placeholder (~datum m-cities3/node))
       #'(→ (Listof (Listof String)) m-cities3/node16/placeholder-type))
      ((_ #:make-placeholder (~datum m-streets4/node))
       #'(→ (Listof String) m-streets4/node17/placeholder-type))
      ((_ #:placeholder (~datum City)) #'City14/placeholder-type)
      ((_ #:placeholder (~datum Street)) #'Street15/placeholder-type)
      ((_ #:placeholder (~datum m-cities3/node))
       #'m-cities3/node16/placeholder-type)
      ((_ #:placeholder (~datum m-streets4/node))
       #'m-streets4/node17/placeholder-type)))
   #:call
   (λ (stx)
     (syntax-parse
      stx
      ((_ #:λroot (~datum City)) #'City2/constructor)
      ((_ #:λroot (~datum Street)) #'Street3/constructor)
      ((_ #:λroot (~datum m-cities3/node)) #'m-cities3/node4/constructor)
      ((_ #:λroot (~datum m-streets4/node)) #'m-streets4/node5/constructor)
      ((_ #:root (~datum City) . rest)
       (syntax/loc stx (City2/constructor . rest)))
      ((_ #:root (~datum Street) . rest)
       (syntax/loc stx (Street3/constructor . rest)))
      ((_ #:root (~datum m-cities3/node) . rest)
       (syntax/loc stx (m-cities3/node4/constructor . rest)))
      ((_ #:root (~datum m-streets4/node) . rest)
       (syntax/loc stx (m-streets4/node5/constructor . rest)))
      ((_ . rest) (syntax/loc stx (City2/constructor . rest)))))
   #:id
   (λ (stx) #'City2/constructor))
  (begin
    (struct (A) City10/placeholder-struct ((f : A)))
    (define-type
     City14/placeholder-type
     (City10/placeholder-struct
      (List
       (U
        (first-step #:placeholder m-streets4/node)
        (Listof (first-step #:placeholder Street)))))))
  (begin
    (struct (A) Street11/placeholder-struct ((f : A)))
    (define-type
     Street15/placeholder-type
     (Street11/placeholder-struct (List String))))
  (begin
    (struct (A) m-cities3/node12/placeholder-struct ((f : A)))
    (define-type
     m-cities3/node16/placeholder-type
     (m-cities3/node12/placeholder-struct (List (Listof (Listof String))))))
  (begin
    (struct (A) m-streets4/node13/placeholder-struct ((f : A)))
    (define-type
     m-streets4/node17/placeholder-type
     (m-streets4/node13/placeholder-struct (List (Listof String)))))
  (begin
    (:
     City6/make-placeholder
     (→
      (U
       (first-step #:placeholder m-streets4/node)
       (Listof (first-step #:placeholder Street)))
      City14/placeholder-type))
    (define (City6/make-placeholder streets)
      ((inst
        City10/placeholder-struct
        (List
         (U
          (first-step #:placeholder m-streets4/node)
          (Listof (first-step #:placeholder Street)))))
       (list streets))))
  (begin
    (: Street7/make-placeholder (→ String Street15/placeholder-type))
    (define (Street7/make-placeholder sname)
      ((inst Street11/placeholder-struct (List String)) (list sname))))
  (begin
    (:
     m-cities3/node8/make-placeholder
     (→ (Listof (Listof String)) m-cities3/node16/placeholder-type))
    (define (m-cities3/node8/make-placeholder cnames)
      ((inst
        m-cities3/node12/placeholder-struct
        (List (Listof (Listof String))))
       (list cnames))))
  (begin
    (:
     m-streets4/node9/make-placeholder
     (→ (Listof String) m-streets4/node17/placeholder-type))
    (define (m-streets4/node9/make-placeholder snames)
      ((inst m-streets4/node13/placeholder-struct (List (Listof String)))
       (list snames))))
  (begin
    (define-type City50/index-type (List 'City46/with-indices-tag2 Index))
    (define-type
     City34/with-indices-type
     (List
      'City42/with-indices-tag
      (U m-streets4/node53/index-type (Listof Street51/index-type))))
    (:
     City38/make-with-indices
     (→
      (U m-streets4/node53/index-type (Listof Street51/index-type))
      City34/with-indices-type))
    (define (City38/make-with-indices streets)
      (list 'City42/with-indices-tag streets)))
  (begin
    (define-type Street51/index-type (List 'Street47/with-indices-tag2 Index))
    (define-type
     Street35/with-indices-type
     (List 'Street43/with-indices-tag String))
    (: Street39/make-with-indices (→ String Street35/with-indices-type))
    (define (Street39/make-with-indices sname)
      (list 'Street43/with-indices-tag sname)))
  (begin
    (define-type
     m-cities3/node52/index-type
     (List 'm-cities3/node48/with-indices-tag2 Index))
    (define-type
     m-cities3/node36/with-indices-type
     (List 'm-cities3/node44/with-indices-tag (Listof City50/index-type)))
    (:
     m-cities3/node40/make-with-indices
     (→ (Listof City50/index-type) m-cities3/node36/with-indices-type))
    (define (m-cities3/node40/make-with-indices returned)
      (list 'm-cities3/node44/with-indices-tag returned)))
  (begin
    (define-type
     m-streets4/node53/index-type
     (List 'm-streets4/node49/with-indices-tag2 Index))
    (define-type
     m-streets4/node37/with-indices-type
     (List 'm-streets4/node45/with-indices-tag (Listof Street51/index-type)))
    (:
     m-streets4/node41/make-with-indices
     (→ (Listof Street51/index-type) m-streets4/node37/with-indices-type))
    (define (m-streets4/node41/make-with-indices returned)
      (list 'm-streets4/node45/with-indices-tag returned)))
  (begin
    (define-type
     City58/with-promises-type
     (tagged
      City66/with-promises-tag
      (streets
       :
       (U
        (Promise m-streets4/node61/with-promises-type)
        (Listof (Promise Street59/with-promises-type))))))
    (:
     City62/make-with-promises
     (→
      (U
       (Promise m-streets4/node61/with-promises-type)
       (Listof (Promise Street59/with-promises-type)))
      City58/with-promises-type))
    (define (City62/make-with-promises streets78/value)
      (tagged
       City66/with-promises-tag
       (streets
        :
        (U
         (Promise m-streets4/node61/with-promises-type)
         (Listof (Promise Street59/with-promises-type)))
        streets78/value))))
  (begin
    (define-type
     Street59/with-promises-type
     (tagged Street67/with-promises-tag (sname : String)))
    (: Street63/make-with-promises (→ String Street59/with-promises-type))
    (define (Street63/make-with-promises sname79/value)
      (tagged Street67/with-promises-tag (sname : String sname79/value))))
  (begin
    (define-type
     m-cities3/node60/with-promises-type
     (tagged
      m-cities3/node68/with-promises-tag
      (returned : (Listof (Promise City58/with-promises-type)))))
    (:
     m-cities3/node64/make-with-promises
     (→
      (Listof (Promise City58/with-promises-type))
      m-cities3/node60/with-promises-type))
    (define (m-cities3/node64/make-with-promises returned80/value)
      (tagged
       m-cities3/node68/with-promises-tag
       (returned
        :
        (Listof (Promise City58/with-promises-type))
        returned80/value))))
  (begin
    (define-type
     m-streets4/node61/with-promises-type
     (tagged
      m-streets4/node69/with-promises-tag
      (returned : (Listof (Promise Street59/with-promises-type)))))
    (:
     m-streets4/node65/make-with-promises
     (→
      (Listof (Promise Street59/with-promises-type))
      m-streets4/node61/with-promises-type))
    (define (m-streets4/node65/make-with-promises returned81/value)
      (tagged
       m-streets4/node69/with-promises-tag
       (returned
        :
        (Listof (Promise Street59/with-promises-type))
        returned81/value))))
  (begin
    (define-type
     City22/incomplete-type
     (List
      'City30/incomplete-tag
      (U
       m-streets4/node17/placeholder-type
       (Listof Street15/placeholder-type))))
    (:
     City26/make-incomplete
     (→
      (U m-streets4/node17/placeholder-type (Listof Street15/placeholder-type))
      City22/incomplete-type))
    (define (City26/make-incomplete streets)
      (list 'City30/incomplete-tag streets)))
  (begin
    (define-type
     Street23/incomplete-type
     (List 'Street31/incomplete-tag String))
    (: Street27/make-incomplete (→ String Street23/incomplete-type))
    (define (Street27/make-incomplete sname)
      (list 'Street31/incomplete-tag sname)))
  (begin
    (define-type
     m-cities3/node24/incomplete-type
     (List 'm-cities3/node32/incomplete-tag (Listof City14/placeholder-type)))
    (:
     m-cities3/node28/make-incomplete
     (→ (Listof City14/placeholder-type) m-cities3/node24/incomplete-type))
    (define (m-cities3/node28/make-incomplete returned)
      (list 'm-cities3/node32/incomplete-tag returned)))
  (begin
    (define-type
     m-streets4/node25/incomplete-type
     (List
      'm-streets4/node33/incomplete-tag
      (Listof Street15/placeholder-type)))
    (:
     m-streets4/node29/make-incomplete
     (→ (Listof Street15/placeholder-type) m-streets4/node25/incomplete-type))
    (define (m-streets4/node29/make-incomplete returned)
      (list 'm-streets4/node33/incomplete-tag returned)))
  (begin
    (:
     City70/mapping-function
     (→
      (U
       (first-step #:placeholder m-streets4/node)
       (Listof (first-step #:placeholder Street)))
      City22/incomplete-type))
    (define City70/mapping-function
      (let ((City1/simple-mapping City6/make-placeholder)
            (Street2/simple-mapping Street7/make-placeholder)
            (m-cities m-cities3/node8/make-placeholder)
            (m-streets m-streets4/node9/make-placeholder)
            (City City26/make-incomplete)
            (Street Street27/make-incomplete)
            (m-cities3/node m-cities3/node28/make-incomplete)
            (m-streets4/node m-streets4/node29/make-incomplete))
        (λ ((streets
             :
             (U
              (first-step #:placeholder m-streets4/node)
              (Listof (first-step #:placeholder Street)))))
          :
          City22/incomplete-type
          (City streets)))))
  (begin
    (: Street71/mapping-function (→ String Street23/incomplete-type))
    (define Street71/mapping-function
      (let ((City1/simple-mapping City6/make-placeholder)
            (Street2/simple-mapping Street7/make-placeholder)
            (m-cities m-cities3/node8/make-placeholder)
            (m-streets m-streets4/node9/make-placeholder)
            (City City26/make-incomplete)
            (Street Street27/make-incomplete)
            (m-cities3/node m-cities3/node28/make-incomplete)
            (m-streets4/node m-streets4/node29/make-incomplete))
        (λ ((sname : String)) : Street23/incomplete-type (Street sname)))))
  (begin
    (:
     m-cities3/node72/mapping-function
     (→ (Listof (Listof String)) m-cities3/node24/incomplete-type))
    (define m-cities3/node72/mapping-function
      (let ((City1/simple-mapping City6/make-placeholder)
            (Street2/simple-mapping Street7/make-placeholder)
            (m-cities m-cities3/node8/make-placeholder)
            (m-streets m-streets4/node9/make-placeholder)
            (City City26/make-incomplete)
            (Street Street27/make-incomplete)
            (m-cities3/node m-cities3/node28/make-incomplete)
            (m-streets4/node m-streets4/node29/make-incomplete))
        (λ ((cnames : (Listof (Listof String))))
          :
          m-cities3/node24/incomplete-type
          (m-cities3/node
           (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
             (define (strings→city (s : (Listof String)))
               :
               (first-step #:placeholder City)
               (City (m-streets s)))
             (map strings→city cnames)))))))
  (begin
    (:
     m-streets4/node73/mapping-function
     (→ (Listof String) m-streets4/node25/incomplete-type))
    (define m-streets4/node73/mapping-function
      (let ((City1/simple-mapping City6/make-placeholder)
            (Street2/simple-mapping Street7/make-placeholder)
            (m-cities m-cities3/node8/make-placeholder)
            (m-streets m-streets4/node9/make-placeholder)
            (City City26/make-incomplete)
            (Street Street27/make-incomplete)
            (m-cities3/node m-cities3/node28/make-incomplete)
            (m-streets4/node m-streets4/node29/make-incomplete))
        (λ ((snames : (Listof String)))
          :
          m-streets4/node25/incomplete-type
          (m-streets4/node
           (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
             (map Street snames)))))))
  (:
   fq
   (case→
    (→
     'City18/placeholder-queue
     City14/placeholder-type
     (List
      (Vectorof City34/with-indices-type)
      (Vectorof Street35/with-indices-type)
      (Vectorof m-cities3/node36/with-indices-type)
      (Vectorof m-streets4/node37/with-indices-type)))
    (→
     'Street19/placeholder-queue
     Street15/placeholder-type
     (List
      (Vectorof City34/with-indices-type)
      (Vectorof Street35/with-indices-type)
      (Vectorof m-cities3/node36/with-indices-type)
      (Vectorof m-streets4/node37/with-indices-type)))
    (→
     'm-cities3/node20/placeholder-queue
     m-cities3/node16/placeholder-type
     (List
      (Vectorof City34/with-indices-type)
      (Vectorof Street35/with-indices-type)
      (Vectorof m-cities3/node36/with-indices-type)
      (Vectorof m-streets4/node37/with-indices-type)))
    (→
     'm-streets4/node21/placeholder-queue
     m-streets4/node17/placeholder-type
     (List
      (Vectorof City34/with-indices-type)
      (Vectorof Street35/with-indices-type)
      (Vectorof m-cities3/node36/with-indices-type)
      (Vectorof m-streets4/node37/with-indices-type)))))
  (define (fq queue-name placeholder)
    (fold-queues
     #:root
     queue-name
     placeholder
     ((City18/placeholder-queue
       (e : City14/placeholder-type)
       (Δ-queues : Δ-Queues)
       enqueue)
      :
      City34/with-indices-type
      (let ((mapping-result
             (apply
              City70/mapping-function
              ((struct-accessor City10/placeholder-struct 0) e)))
            (f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List
                  (U
                   m-streets4/node17/placeholder-type
                   (Listof Street15/placeholder-type)))
                 Δ-Queues
                 (City14/placeholder-type
                  (List 'City46/with-indices-tag2 Index)
                  (struct-predicate City10/placeholder-struct)
                  λ…)
                 (Street15/placeholder-type
                  (List 'Street47/with-indices-tag2 Index)
                  (struct-predicate Street11/placeholder-struct)
                  λ…)
                 (m-cities3/node16/placeholder-type
                  (List 'm-cities3/node48/with-indices-tag2 Index)
                  (struct-predicate m-cities3/node12/placeholder-struct)
                  λ…)
                 (m-streets4/node17/placeholder-type
                  (List 'm-streets4/node49/with-indices-tag2 Index)
                  (struct-predicate m-streets4/node13/placeholder-struct)
                  λ…))
               (λ ((val
                    :
                    (List
                     (U
                      m-streets4/node17/placeholder-type
                      (Listof Street15/placeholder-type))))
                   (acc : Δ-Queues))
                 :
                 (values
                  (List
                   (U
                    (List 'm-streets4/node49/with-indices-tag2 Index)
                    (Listof (List 'Street47/with-indices-tag2 Index))))
                  Δ-Queues)
                 (let*-values (((temp83) (apply values val))
                               ((temp84 temp85)
                                ((λ ((val
                                      :
                                      (U
                                       m-streets4/node17/placeholder-type
                                       (Listof Street15/placeholder-type)))
                                     (acc : Δ-Queues))
                                   :
                                   (values
                                    (U
                                     (List
                                      'm-streets4/node49/with-indices-tag2
                                      Index)
                                     (Listof
                                      (List
                                       'Street47/with-indices-tag2
                                       Index)))
                                    Δ-Queues)
                                   (cond
                                    (((struct-predicate
                                       m-streets4/node13/placeholder-struct)
                                      val)
                                     ((ann
                                       (λ ((p
                                            :
                                            m-streets4/node17/placeholder-type)
                                           (Δ-acc : Δ-Queues))
                                         :
                                         (values
                                          (List
                                           'm-streets4/node49/with-indices-tag2
                                           Index)
                                          Δ-Queues)
                                         (%
                                          index
                                          new-Δ-acc
                                          =
                                          (enqueue
                                           'm-streets4/node21/placeholder-queue
                                           p
                                           Δ-acc)
                                          (values
                                           (list
                                            'm-streets4/node49/with-indices-tag2
                                            index)
                                           new-Δ-acc)))
                                       (→
                                        m-streets4/node17/placeholder-type
                                        Δ-Queues
                                        (values
                                         (List
                                          'm-streets4/node49/with-indices-tag2
                                          Index)
                                         Δ-Queues)))
                                      val
                                      acc))
                                    (#t
                                     ((λ ((val
                                           :
                                           (Listof Street15/placeholder-type))
                                          (acc : Δ-Queues))
                                        :
                                        (values
                                         (Listof
                                          (List
                                           'Street47/with-indices-tag2
                                           Index))
                                         Δ-Queues)
                                        (let ((f
                                               ((inst
                                                 foldl
                                                 Street15/placeholder-type
                                                 (Pairof
                                                  (Listof
                                                   (List
                                                    'Street47/with-indices-tag2
                                                    Index))
                                                  Δ-Queues)
                                                 Nothing
                                                 Nothing)
                                                (λ ((x
                                                     :
                                                     Street15/placeholder-type)
                                                    (acc1
                                                     :
                                                     (Pairof
                                                      (Listof
                                                       (List
                                                        'Street47/with-indices-tag2
                                                        Index))
                                                      Δ-Queues)))
                                                  (let-values (((res res-acc)
                                                                ((ann
                                                                  (λ ((p
                                                                       :
                                                                       Street15/placeholder-type)
                                                                      (Δ-acc
                                                                       :
                                                                       Δ-Queues))
                                                                    :
                                                                    (values
                                                                     (List
                                                                      'Street47/with-indices-tag2
                                                                      Index)
                                                                     Δ-Queues)
                                                                    (%
                                                                     index
                                                                     new-Δ-acc
                                                                     =
                                                                     (enqueue
                                                                      'Street19/placeholder-queue
                                                                      p
                                                                      Δ-acc)
                                                                     (values
                                                                      (list
                                                                       'Street47/with-indices-tag2
                                                                       index)
                                                                      new-Δ-acc)))
                                                                  (→
                                                                   Street15/placeholder-type
                                                                   Δ-Queues
                                                                   (values
                                                                    (List
                                                                     'Street47/with-indices-tag2
                                                                     Index)
                                                                    Δ-Queues)))
                                                                 x
                                                                 (cdr acc1))))
                                                    (cons
                                                     (cons res (car acc1))
                                                     res-acc)))
                                                (cons '() acc)
                                                val)))
                                          (values (reverse (car f)) (cdr f))))
                                      val
                                      acc))
                                    (else
                                     (typecheck-fail
                                      (U
                                       m-streets4/node17/placeholder-type
                                       (Listof Street15/placeholder-type))
                                      "Unhandled union case in (U m-streets4/node17/placeholder-type (Listof Street15/placeholder-type)), whole type was:(List (U m-streets4/node17/placeholder-type (Listof Street15/placeholder-type)))"))))
                                 temp83
                                 acc)))
                   (values (list temp84) temp85))))))
        (let-values (((r new-Δ-queues) (f (cdr mapping-result) Δ-queues)))
          (values (apply City38/make-with-indices r) new-Δ-queues))))
     ((Street19/placeholder-queue
       (e : Street15/placeholder-type)
       (Δ-queues : Δ-Queues)
       enqueue)
      :
      Street35/with-indices-type
      (let ((mapping-result
             (apply
              Street71/mapping-function
              ((struct-accessor Street11/placeholder-struct 0) e)))
            (f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List String)
                 Δ-Queues
                 (City14/placeholder-type
                  (List 'City46/with-indices-tag2 Index)
                  (struct-predicate City10/placeholder-struct)
                  λ…)
                 (Street15/placeholder-type
                  (List 'Street47/with-indices-tag2 Index)
                  (struct-predicate Street11/placeholder-struct)
                  λ…)
                 (m-cities3/node16/placeholder-type
                  (List 'm-cities3/node48/with-indices-tag2 Index)
                  (struct-predicate m-cities3/node12/placeholder-struct)
                  λ…)
                 (m-streets4/node17/placeholder-type
                  (List 'm-streets4/node49/with-indices-tag2 Index)
                  (struct-predicate m-streets4/node13/placeholder-struct)
                  λ…))
               (λ ((val : (List String)) (acc : Δ-Queues))
                 :
                 (values (List String) Δ-Queues)
                 (let*-values (((String93) (apply values val))
                               ((String94 String95)
                                ((inst values String Δ-Queues) String93 acc)))
                   (values (list String94) String95))))))
        (let-values (((r new-Δ-queues) (f (cdr mapping-result) Δ-queues)))
          (values (apply Street39/make-with-indices r) new-Δ-queues))))
     ((m-cities3/node20/placeholder-queue
       (e : m-cities3/node16/placeholder-type)
       (Δ-queues : Δ-Queues)
       enqueue)
      :
      m-cities3/node36/with-indices-type
      (let ((mapping-result
             (apply
              m-cities3/node72/mapping-function
              ((struct-accessor m-cities3/node12/placeholder-struct 0) e)))
            (f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof City14/placeholder-type))
                 Δ-Queues
                 (City14/placeholder-type
                  (List 'City46/with-indices-tag2 Index)
                  (struct-predicate City10/placeholder-struct)
                  λ…)
                 (Street15/placeholder-type
                  (List 'Street47/with-indices-tag2 Index)
                  (struct-predicate Street11/placeholder-struct)
                  λ…)
                 (m-cities3/node16/placeholder-type
                  (List 'm-cities3/node48/with-indices-tag2 Index)
                  (struct-predicate m-cities3/node12/placeholder-struct)
                  λ…)
                 (m-streets4/node17/placeholder-type
                  (List 'm-streets4/node49/with-indices-tag2 Index)
                  (struct-predicate m-streets4/node13/placeholder-struct)
                  λ…))
               (λ ((val : (List (Listof City14/placeholder-type)))
                   (acc : Δ-Queues))
                 :
                 (values
                  (List (Listof (List 'City46/with-indices-tag2 Index)))
                  Δ-Queues)
                 (let*-values (((temp98) (apply values val))
                               ((temp99 temp100)
                                ((λ ((val : (Listof City14/placeholder-type))
                                     (acc : Δ-Queues))
                                   :
                                   (values
                                    (Listof
                                     (List 'City46/with-indices-tag2 Index))
                                    Δ-Queues)
                                   (let ((f
                                          ((inst
                                            foldl
                                            City14/placeholder-type
                                            (Pairof
                                             (Listof
                                              (List
                                               'City46/with-indices-tag2
                                               Index))
                                             Δ-Queues)
                                            Nothing
                                            Nothing)
                                           (λ ((x : City14/placeholder-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (List
                                                   'City46/with-indices-tag2
                                                   Index))
                                                 Δ-Queues)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((p
                                                                  :
                                                                  City14/placeholder-type)
                                                                 (Δ-acc
                                                                  :
                                                                  Δ-Queues))
                                                               :
                                                               (values
                                                                (List
                                                                 'City46/with-indices-tag2
                                                                 Index)
                                                                Δ-Queues)
                                                               (%
                                                                index
                                                                new-Δ-acc
                                                                =
                                                                (enqueue
                                                                 'City18/placeholder-queue
                                                                 p
                                                                 Δ-acc)
                                                                (values
                                                                 (list
                                                                  'City46/with-indices-tag2
                                                                  index)
                                                                 new-Δ-acc)))
                                                             (→
                                                              City14/placeholder-type
                                                              Δ-Queues
                                                              (values
                                                               (List
                                                                'City46/with-indices-tag2
                                                                Index)
                                                               Δ-Queues)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp98
                                 acc)))
                   (values (list temp99) temp100))))))
        (let-values (((r new-Δ-queues) (f (cdr mapping-result) Δ-queues)))
          (values (apply m-cities3/node40/make-with-indices r) new-Δ-queues))))
     ((m-streets4/node21/placeholder-queue
       (e : m-streets4/node17/placeholder-type)
       (Δ-queues : Δ-Queues)
       enqueue)
      :
      m-streets4/node37/with-indices-type
      (let ((mapping-result
             (apply
              m-streets4/node73/mapping-function
              ((struct-accessor m-streets4/node13/placeholder-struct 0) e)))
            (f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof Street15/placeholder-type))
                 Δ-Queues
                 (City14/placeholder-type
                  (List 'City46/with-indices-tag2 Index)
                  (struct-predicate City10/placeholder-struct)
                  λ…)
                 (Street15/placeholder-type
                  (List 'Street47/with-indices-tag2 Index)
                  (struct-predicate Street11/placeholder-struct)
                  λ…)
                 (m-cities3/node16/placeholder-type
                  (List 'm-cities3/node48/with-indices-tag2 Index)
                  (struct-predicate m-cities3/node12/placeholder-struct)
                  λ…)
                 (m-streets4/node17/placeholder-type
                  (List 'm-streets4/node49/with-indices-tag2 Index)
                  (struct-predicate m-streets4/node13/placeholder-struct)
                  λ…))
               (λ ((val : (List (Listof Street15/placeholder-type)))
                   (acc : Δ-Queues))
                 :
                 (values
                  (List (Listof (List 'Street47/with-indices-tag2 Index)))
                  Δ-Queues)
                 (let*-values (((temp105) (apply values val))
                               ((temp106 temp107)
                                ((λ ((val : (Listof Street15/placeholder-type))
                                     (acc : Δ-Queues))
                                   :
                                   (values
                                    (Listof
                                     (List 'Street47/with-indices-tag2 Index))
                                    Δ-Queues)
                                   (let ((f
                                          ((inst
                                            foldl
                                            Street15/placeholder-type
                                            (Pairof
                                             (Listof
                                              (List
                                               'Street47/with-indices-tag2
                                               Index))
                                             Δ-Queues)
                                            Nothing
                                            Nothing)
                                           (λ ((x : Street15/placeholder-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (List
                                                   'Street47/with-indices-tag2
                                                   Index))
                                                 Δ-Queues)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((p
                                                                  :
                                                                  Street15/placeholder-type)
                                                                 (Δ-acc
                                                                  :
                                                                  Δ-Queues))
                                                               :
                                                               (values
                                                                (List
                                                                 'Street47/with-indices-tag2
                                                                 Index)
                                                                Δ-Queues)
                                                               (%
                                                                index
                                                                new-Δ-acc
                                                                =
                                                                (enqueue
                                                                 'Street19/placeholder-queue
                                                                 p
                                                                 Δ-acc)
                                                                (values
                                                                 (list
                                                                  'Street47/with-indices-tag2
                                                                  index)
                                                                 new-Δ-acc)))
                                                             (→
                                                              Street15/placeholder-type
                                                              Δ-Queues
                                                              (values
                                                               (List
                                                                'Street47/with-indices-tag2
                                                                Index)
                                                               Δ-Queues)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp105
                                 acc)))
                   (values (list temp106) temp107))))))
        (let-values (((r new-Δ-queues) (f (cdr mapping-result) Δ-queues)))
          (values
           (apply m-streets4/node41/make-with-indices r)
           new-Δ-queues))))))
  (begin
    (:
     City2/constructor
     (→
      (U
       (first-step #:placeholder m-streets4/node)
       (Listof (first-step #:placeholder Street)))
      (Promise City58/with-promises-type)))
    (define (City2/constructor streets)
      (match-let
       (((list
          City74/database
          Street75/database
          m-cities3/node76/database
          m-streets4/node77/database)
         (fq 'City18/placeholder-queue (City6/make-placeholder streets))))
       (begin
         (:
          City54/with-indices→with-promises
          (→ City34/with-indices-type City58/with-promises-type))
         (define (City54/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List
                  (U
                   m-streets4/node53/index-type
                   (Listof Street51/index-type)))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val
                    :
                    (List
                     (U
                      m-streets4/node53/index-type
                      (Listof Street51/index-type))))
                   (acc : Void))
                 :
                 (values
                  (List
                   (U
                    (Promise m-streets4/node61/with-promises-type)
                    (Listof (Promise Street59/with-promises-type))))
                  Void)
                 (let*-values (((temp112) (apply values val))
                               ((temp113 temp114)
                                ((λ ((val
                                      :
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type)))
                                     (acc : Void))
                                   :
                                   (values
                                    (U
                                     (Promise
                                      m-streets4/node61/with-promises-type)
                                     (Listof
                                      (Promise Street59/with-promises-type)))
                                    Void)
                                   (cond
                                    (((λ (x)
                                        (and (pair? x)
                                             (eq?
                                              (car x)
                                              'm-streets4/node49/with-indices-tag2)))
                                      val)
                                     ((ann
                                       (λ ((tagged-index
                                            :
                                            m-streets4/node53/index-type)
                                           (acc : Void))
                                         :
                                         (values
                                          (Promise
                                           m-streets4/node61/with-promises-type)
                                          Void)
                                         (values
                                          (let ((successor-with-index
                                                 (vector-ref
                                                  m-streets4/node77/database
                                                  (cadr tagged-index))))
                                            (delay
                                             (m-streets4/node57/with-indices→with-promises
                                              successor-with-index)))
                                          acc))
                                       (→
                                        m-streets4/node53/index-type
                                        Void
                                        (values
                                         (Promise
                                          m-streets4/node61/with-promises-type)
                                         Void)))
                                      val
                                      acc))
                                    (#t
                                     ((λ ((val : (Listof Street51/index-type))
                                          (acc : Void))
                                        :
                                        (values
                                         (Listof
                                          (Promise
                                           Street59/with-promises-type))
                                         Void)
                                        (let ((f
                                               ((inst
                                                 foldl
                                                 Street51/index-type
                                                 (Pairof
                                                  (Listof
                                                   (Promise
                                                    Street59/with-promises-type))
                                                  Void)
                                                 Nothing
                                                 Nothing)
                                                (λ ((x : Street51/index-type)
                                                    (acc1
                                                     :
                                                     (Pairof
                                                      (Listof
                                                       (Promise
                                                        Street59/with-promises-type))
                                                      Void)))
                                                  (let-values (((res res-acc)
                                                                ((ann
                                                                  (λ ((tagged-index
                                                                       :
                                                                       Street51/index-type)
                                                                      (acc
                                                                       :
                                                                       Void))
                                                                    :
                                                                    (values
                                                                     (Promise
                                                                      Street59/with-promises-type)
                                                                     Void)
                                                                    (values
                                                                     (let ((successor-with-index
                                                                            (vector-ref
                                                                             Street75/database
                                                                             (cadr
                                                                              tagged-index))))
                                                                       (delay
                                                                        (Street55/with-indices→with-promises
                                                                         successor-with-index)))
                                                                     acc))
                                                                  (→
                                                                   Street51/index-type
                                                                   Void
                                                                   (values
                                                                    (Promise
                                                                     Street59/with-promises-type)
                                                                    Void)))
                                                                 x
                                                                 (cdr acc1))))
                                                    (cons
                                                     (cons res (car acc1))
                                                     res-acc)))
                                                (cons '() acc)
                                                val)))
                                          (values (reverse (car f)) (cdr f))))
                                      val
                                      acc))
                                    (else
                                     (typecheck-fail
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type))
                                      "Unhandled union case in (U m-streets4/node53/index-type (Listof Street51/index-type)), whole type was:(List (U m-streets4/node53/index-type (Listof Street51/index-type)))"))))
                                 temp112
                                 acc)))
                   (values (list temp113) temp114)))))
           (apply City62/make-with-promises (first-value (f (cdr n) (void))))))
       (begin
         (:
          Street55/with-indices→with-promises
          (→ Street35/with-indices-type Street59/with-promises-type))
         (define (Street55/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List String)
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List String)) (acc : Void))
                 :
                 (values (List String) Void)
                 (let*-values (((String122) (apply values val))
                               ((String123 String124)
                                ((inst values String Void) String122 acc)))
                   (values (list String123) String124)))))
           (apply
            Street63/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-cities3/node56/with-indices→with-promises
          (→
           m-cities3/node36/with-indices-type
           m-cities3/node60/with-promises-type))
         (define (m-cities3/node56/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof City50/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof City50/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise City58/with-promises-type)))
                  Void)
                 (let*-values (((temp127) (apply values val))
                               ((temp128 temp129)
                                ((λ ((val : (Listof City50/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise City58/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            City50/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               City58/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : City50/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   City58/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  City50/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 City58/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        City74/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (City54/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              City50/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                City58/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp127
                                 acc)))
                   (values (list temp128) temp129)))))
           (apply
            m-cities3/node64/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-streets4/node57/with-indices→with-promises
          (→
           m-streets4/node37/with-indices-type
           m-streets4/node61/with-promises-type))
         (define (m-streets4/node57/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof Street51/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof Street51/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise Street59/with-promises-type)))
                  Void)
                 (let*-values (((temp134) (apply values val))
                               ((temp135 temp136)
                                ((λ ((val : (Listof Street51/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise Street59/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            Street51/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               Street59/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : Street51/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   Street59/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  Street51/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 Street59/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        Street75/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (Street55/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              Street51/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                Street59/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp134
                                 acc)))
                   (values (list temp135) temp136)))))
           (apply
            m-streets4/node65/make-with-promises
            (first-value (f (cdr n) (void))))))
       (delay
        (City54/with-indices→with-promises (vector-ref City74/database 0))))))
  (begin
    (: Street3/constructor (→ String (Promise Street59/with-promises-type)))
    (define (Street3/constructor sname)
      (match-let
       (((list
          City74/database
          Street75/database
          m-cities3/node76/database
          m-streets4/node77/database)
         (fq 'Street19/placeholder-queue (Street7/make-placeholder sname))))
       (begin
         (:
          City54/with-indices→with-promises
          (→ City34/with-indices-type City58/with-promises-type))
         (define (City54/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List
                  (U
                   m-streets4/node53/index-type
                   (Listof Street51/index-type)))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val
                    :
                    (List
                     (U
                      m-streets4/node53/index-type
                      (Listof Street51/index-type))))
                   (acc : Void))
                 :
                 (values
                  (List
                   (U
                    (Promise m-streets4/node61/with-promises-type)
                    (Listof (Promise Street59/with-promises-type))))
                  Void)
                 (let*-values (((temp141) (apply values val))
                               ((temp142 temp143)
                                ((λ ((val
                                      :
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type)))
                                     (acc : Void))
                                   :
                                   (values
                                    (U
                                     (Promise
                                      m-streets4/node61/with-promises-type)
                                     (Listof
                                      (Promise Street59/with-promises-type)))
                                    Void)
                                   (cond
                                    (((λ (x)
                                        (and (pair? x)
                                             (eq?
                                              (car x)
                                              'm-streets4/node49/with-indices-tag2)))
                                      val)
                                     ((ann
                                       (λ ((tagged-index
                                            :
                                            m-streets4/node53/index-type)
                                           (acc : Void))
                                         :
                                         (values
                                          (Promise
                                           m-streets4/node61/with-promises-type)
                                          Void)
                                         (values
                                          (let ((successor-with-index
                                                 (vector-ref
                                                  m-streets4/node77/database
                                                  (cadr tagged-index))))
                                            (delay
                                             (m-streets4/node57/with-indices→with-promises
                                              successor-with-index)))
                                          acc))
                                       (→
                                        m-streets4/node53/index-type
                                        Void
                                        (values
                                         (Promise
                                          m-streets4/node61/with-promises-type)
                                         Void)))
                                      val
                                      acc))
                                    (#t
                                     ((λ ((val : (Listof Street51/index-type))
                                          (acc : Void))
                                        :
                                        (values
                                         (Listof
                                          (Promise
                                           Street59/with-promises-type))
                                         Void)
                                        (let ((f
                                               ((inst
                                                 foldl
                                                 Street51/index-type
                                                 (Pairof
                                                  (Listof
                                                   (Promise
                                                    Street59/with-promises-type))
                                                  Void)
                                                 Nothing
                                                 Nothing)
                                                (λ ((x : Street51/index-type)
                                                    (acc1
                                                     :
                                                     (Pairof
                                                      (Listof
                                                       (Promise
                                                        Street59/with-promises-type))
                                                      Void)))
                                                  (let-values (((res res-acc)
                                                                ((ann
                                                                  (λ ((tagged-index
                                                                       :
                                                                       Street51/index-type)
                                                                      (acc
                                                                       :
                                                                       Void))
                                                                    :
                                                                    (values
                                                                     (Promise
                                                                      Street59/with-promises-type)
                                                                     Void)
                                                                    (values
                                                                     (let ((successor-with-index
                                                                            (vector-ref
                                                                             Street75/database
                                                                             (cadr
                                                                              tagged-index))))
                                                                       (delay
                                                                        (Street55/with-indices→with-promises
                                                                         successor-with-index)))
                                                                     acc))
                                                                  (→
                                                                   Street51/index-type
                                                                   Void
                                                                   (values
                                                                    (Promise
                                                                     Street59/with-promises-type)
                                                                    Void)))
                                                                 x
                                                                 (cdr acc1))))
                                                    (cons
                                                     (cons res (car acc1))
                                                     res-acc)))
                                                (cons '() acc)
                                                val)))
                                          (values (reverse (car f)) (cdr f))))
                                      val
                                      acc))
                                    (else
                                     (typecheck-fail
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type))
                                      "Unhandled union case in (U m-streets4/node53/index-type (Listof Street51/index-type)), whole type was:(List (U m-streets4/node53/index-type (Listof Street51/index-type)))"))))
                                 temp141
                                 acc)))
                   (values (list temp142) temp143)))))
           (apply City62/make-with-promises (first-value (f (cdr n) (void))))))
       (begin
         (:
          Street55/with-indices→with-promises
          (→ Street35/with-indices-type Street59/with-promises-type))
         (define (Street55/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List String)
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List String)) (acc : Void))
                 :
                 (values (List String) Void)
                 (let*-values (((String151) (apply values val))
                               ((String152 String153)
                                ((inst values String Void) String151 acc)))
                   (values (list String152) String153)))))
           (apply
            Street63/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-cities3/node56/with-indices→with-promises
          (→
           m-cities3/node36/with-indices-type
           m-cities3/node60/with-promises-type))
         (define (m-cities3/node56/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof City50/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof City50/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise City58/with-promises-type)))
                  Void)
                 (let*-values (((temp156) (apply values val))
                               ((temp157 temp158)
                                ((λ ((val : (Listof City50/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise City58/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            City50/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               City58/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : City50/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   City58/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  City50/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 City58/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        City74/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (City54/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              City50/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                City58/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp156
                                 acc)))
                   (values (list temp157) temp158)))))
           (apply
            m-cities3/node64/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-streets4/node57/with-indices→with-promises
          (→
           m-streets4/node37/with-indices-type
           m-streets4/node61/with-promises-type))
         (define (m-streets4/node57/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof Street51/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof Street51/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise Street59/with-promises-type)))
                  Void)
                 (let*-values (((temp163) (apply values val))
                               ((temp164 temp165)
                                ((λ ((val : (Listof Street51/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise Street59/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            Street51/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               Street59/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : Street51/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   Street59/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  Street51/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 Street59/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        Street75/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (Street55/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              Street51/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                Street59/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp163
                                 acc)))
                   (values (list temp164) temp165)))))
           (apply
            m-streets4/node65/make-with-promises
            (first-value (f (cdr n) (void))))))
       (delay
        (Street55/with-indices→with-promises
         (vector-ref Street75/database 0))))))
  (begin
    (:
     m-cities3/node4/constructor
     (→
      (Listof (Listof String))
      (Promise m-cities3/node60/with-promises-type)))
    (define (m-cities3/node4/constructor cnames)
      (match-let
       (((list
          City74/database
          Street75/database
          m-cities3/node76/database
          m-streets4/node77/database)
         (fq
          'm-cities3/node20/placeholder-queue
          (m-cities3/node8/make-placeholder cnames))))
       (begin
         (:
          City54/with-indices→with-promises
          (→ City34/with-indices-type City58/with-promises-type))
         (define (City54/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List
                  (U
                   m-streets4/node53/index-type
                   (Listof Street51/index-type)))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val
                    :
                    (List
                     (U
                      m-streets4/node53/index-type
                      (Listof Street51/index-type))))
                   (acc : Void))
                 :
                 (values
                  (List
                   (U
                    (Promise m-streets4/node61/with-promises-type)
                    (Listof (Promise Street59/with-promises-type))))
                  Void)
                 (let*-values (((temp170) (apply values val))
                               ((temp171 temp172)
                                ((λ ((val
                                      :
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type)))
                                     (acc : Void))
                                   :
                                   (values
                                    (U
                                     (Promise
                                      m-streets4/node61/with-promises-type)
                                     (Listof
                                      (Promise Street59/with-promises-type)))
                                    Void)
                                   (cond
                                    (((λ (x)
                                        (and (pair? x)
                                             (eq?
                                              (car x)
                                              'm-streets4/node49/with-indices-tag2)))
                                      val)
                                     ((ann
                                       (λ ((tagged-index
                                            :
                                            m-streets4/node53/index-type)
                                           (acc : Void))
                                         :
                                         (values
                                          (Promise
                                           m-streets4/node61/with-promises-type)
                                          Void)
                                         (values
                                          (let ((successor-with-index
                                                 (vector-ref
                                                  m-streets4/node77/database
                                                  (cadr tagged-index))))
                                            (delay
                                             (m-streets4/node57/with-indices→with-promises
                                              successor-with-index)))
                                          acc))
                                       (→
                                        m-streets4/node53/index-type
                                        Void
                                        (values
                                         (Promise
                                          m-streets4/node61/with-promises-type)
                                         Void)))
                                      val
                                      acc))
                                    (#t
                                     ((λ ((val : (Listof Street51/index-type))
                                          (acc : Void))
                                        :
                                        (values
                                         (Listof
                                          (Promise
                                           Street59/with-promises-type))
                                         Void)
                                        (let ((f
                                               ((inst
                                                 foldl
                                                 Street51/index-type
                                                 (Pairof
                                                  (Listof
                                                   (Promise
                                                    Street59/with-promises-type))
                                                  Void)
                                                 Nothing
                                                 Nothing)
                                                (λ ((x : Street51/index-type)
                                                    (acc1
                                                     :
                                                     (Pairof
                                                      (Listof
                                                       (Promise
                                                        Street59/with-promises-type))
                                                      Void)))
                                                  (let-values (((res res-acc)
                                                                ((ann
                                                                  (λ ((tagged-index
                                                                       :
                                                                       Street51/index-type)
                                                                      (acc
                                                                       :
                                                                       Void))
                                                                    :
                                                                    (values
                                                                     (Promise
                                                                      Street59/with-promises-type)
                                                                     Void)
                                                                    (values
                                                                     (let ((successor-with-index
                                                                            (vector-ref
                                                                             Street75/database
                                                                             (cadr
                                                                              tagged-index))))
                                                                       (delay
                                                                        (Street55/with-indices→with-promises
                                                                         successor-with-index)))
                                                                     acc))
                                                                  (→
                                                                   Street51/index-type
                                                                   Void
                                                                   (values
                                                                    (Promise
                                                                     Street59/with-promises-type)
                                                                    Void)))
                                                                 x
                                                                 (cdr acc1))))
                                                    (cons
                                                     (cons res (car acc1))
                                                     res-acc)))
                                                (cons '() acc)
                                                val)))
                                          (values (reverse (car f)) (cdr f))))
                                      val
                                      acc))
                                    (else
                                     (typecheck-fail
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type))
                                      "Unhandled union case in (U m-streets4/node53/index-type (Listof Street51/index-type)), whole type was:(List (U m-streets4/node53/index-type (Listof Street51/index-type)))"))))
                                 temp170
                                 acc)))
                   (values (list temp171) temp172)))))
           (apply City62/make-with-promises (first-value (f (cdr n) (void))))))
       (begin
         (:
          Street55/with-indices→with-promises
          (→ Street35/with-indices-type Street59/with-promises-type))
         (define (Street55/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List String)
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List String)) (acc : Void))
                 :
                 (values (List String) Void)
                 (let*-values (((String180) (apply values val))
                               ((String181 String182)
                                ((inst values String Void) String180 acc)))
                   (values (list String181) String182)))))
           (apply
            Street63/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-cities3/node56/with-indices→with-promises
          (→
           m-cities3/node36/with-indices-type
           m-cities3/node60/with-promises-type))
         (define (m-cities3/node56/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof City50/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof City50/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise City58/with-promises-type)))
                  Void)
                 (let*-values (((temp185) (apply values val))
                               ((temp186 temp187)
                                ((λ ((val : (Listof City50/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise City58/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            City50/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               City58/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : City50/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   City58/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  City50/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 City58/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        City74/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (City54/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              City50/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                City58/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp185
                                 acc)))
                   (values (list temp186) temp187)))))
           (apply
            m-cities3/node64/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-streets4/node57/with-indices→with-promises
          (→
           m-streets4/node37/with-indices-type
           m-streets4/node61/with-promises-type))
         (define (m-streets4/node57/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof Street51/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof Street51/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise Street59/with-promises-type)))
                  Void)
                 (let*-values (((temp192) (apply values val))
                               ((temp193 temp194)
                                ((λ ((val : (Listof Street51/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise Street59/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            Street51/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               Street59/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : Street51/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   Street59/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  Street51/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 Street59/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        Street75/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (Street55/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              Street51/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                Street59/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp192
                                 acc)))
                   (values (list temp193) temp194)))))
           (apply
            m-streets4/node65/make-with-promises
            (first-value (f (cdr n) (void))))))
       (delay
        (m-cities3/node56/with-indices→with-promises
         (vector-ref m-cities3/node76/database 0))))))
  (begin
    (:
     m-streets4/node5/constructor
     (→ (Listof String) (Promise m-streets4/node61/with-promises-type)))
    (define (m-streets4/node5/constructor snames)
      (match-let
       (((list
          City74/database
          Street75/database
          m-cities3/node76/database
          m-streets4/node77/database)
         (fq
          'm-streets4/node21/placeholder-queue
          (m-streets4/node9/make-placeholder snames))))
       (begin
         (:
          City54/with-indices→with-promises
          (→ City34/with-indices-type City58/with-promises-type))
         (define (City54/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List
                  (U
                   m-streets4/node53/index-type
                   (Listof Street51/index-type)))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val
                    :
                    (List
                     (U
                      m-streets4/node53/index-type
                      (Listof Street51/index-type))))
                   (acc : Void))
                 :
                 (values
                  (List
                   (U
                    (Promise m-streets4/node61/with-promises-type)
                    (Listof (Promise Street59/with-promises-type))))
                  Void)
                 (let*-values (((temp199) (apply values val))
                               ((temp200 temp201)
                                ((λ ((val
                                      :
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type)))
                                     (acc : Void))
                                   :
                                   (values
                                    (U
                                     (Promise
                                      m-streets4/node61/with-promises-type)
                                     (Listof
                                      (Promise Street59/with-promises-type)))
                                    Void)
                                   (cond
                                    (((λ (x)
                                        (and (pair? x)
                                             (eq?
                                              (car x)
                                              'm-streets4/node49/with-indices-tag2)))
                                      val)
                                     ((ann
                                       (λ ((tagged-index
                                            :
                                            m-streets4/node53/index-type)
                                           (acc : Void))
                                         :
                                         (values
                                          (Promise
                                           m-streets4/node61/with-promises-type)
                                          Void)
                                         (values
                                          (let ((successor-with-index
                                                 (vector-ref
                                                  m-streets4/node77/database
                                                  (cadr tagged-index))))
                                            (delay
                                             (m-streets4/node57/with-indices→with-promises
                                              successor-with-index)))
                                          acc))
                                       (→
                                        m-streets4/node53/index-type
                                        Void
                                        (values
                                         (Promise
                                          m-streets4/node61/with-promises-type)
                                         Void)))
                                      val
                                      acc))
                                    (#t
                                     ((λ ((val : (Listof Street51/index-type))
                                          (acc : Void))
                                        :
                                        (values
                                         (Listof
                                          (Promise
                                           Street59/with-promises-type))
                                         Void)
                                        (let ((f
                                               ((inst
                                                 foldl
                                                 Street51/index-type
                                                 (Pairof
                                                  (Listof
                                                   (Promise
                                                    Street59/with-promises-type))
                                                  Void)
                                                 Nothing
                                                 Nothing)
                                                (λ ((x : Street51/index-type)
                                                    (acc1
                                                     :
                                                     (Pairof
                                                      (Listof
                                                       (Promise
                                                        Street59/with-promises-type))
                                                      Void)))
                                                  (let-values (((res res-acc)
                                                                ((ann
                                                                  (λ ((tagged-index
                                                                       :
                                                                       Street51/index-type)
                                                                      (acc
                                                                       :
                                                                       Void))
                                                                    :
                                                                    (values
                                                                     (Promise
                                                                      Street59/with-promises-type)
                                                                     Void)
                                                                    (values
                                                                     (let ((successor-with-index
                                                                            (vector-ref
                                                                             Street75/database
                                                                             (cadr
                                                                              tagged-index))))
                                                                       (delay
                                                                        (Street55/with-indices→with-promises
                                                                         successor-with-index)))
                                                                     acc))
                                                                  (→
                                                                   Street51/index-type
                                                                   Void
                                                                   (values
                                                                    (Promise
                                                                     Street59/with-promises-type)
                                                                    Void)))
                                                                 x
                                                                 (cdr acc1))))
                                                    (cons
                                                     (cons res (car acc1))
                                                     res-acc)))
                                                (cons '() acc)
                                                val)))
                                          (values (reverse (car f)) (cdr f))))
                                      val
                                      acc))
                                    (else
                                     (typecheck-fail
                                      (U
                                       m-streets4/node53/index-type
                                       (Listof Street51/index-type))
                                      "Unhandled union case in (U m-streets4/node53/index-type (Listof Street51/index-type)), whole type was:(List (U m-streets4/node53/index-type (Listof Street51/index-type)))"))))
                                 temp199
                                 acc)))
                   (values (list temp200) temp201)))))
           (apply City62/make-with-promises (first-value (f (cdr n) (void))))))
       (begin
         (:
          Street55/with-indices→with-promises
          (→ Street35/with-indices-type Street59/with-promises-type))
         (define (Street55/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List String)
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List String)) (acc : Void))
                 :
                 (values (List String) Void)
                 (let*-values (((String209) (apply values val))
                               ((String210 String211)
                                ((inst values String Void) String209 acc)))
                   (values (list String210) String211)))))
           (apply
            Street63/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-cities3/node56/with-indices→with-promises
          (→
           m-cities3/node36/with-indices-type
           m-cities3/node60/with-promises-type))
         (define (m-cities3/node56/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof City50/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof City50/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise City58/with-promises-type)))
                  Void)
                 (let*-values (((temp214) (apply values val))
                               ((temp215 temp216)
                                ((λ ((val : (Listof City50/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise City58/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            City50/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               City58/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : City50/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   City58/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  City50/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 City58/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        City74/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (City54/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              City50/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                City58/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp214
                                 acc)))
                   (values (list temp215) temp216)))))
           (apply
            m-cities3/node64/make-with-promises
            (first-value (f (cdr n) (void))))))
       (begin
         (:
          m-streets4/node57/with-indices→with-promises
          (→
           m-streets4/node37/with-indices-type
           m-streets4/node61/with-promises-type))
         (define (m-streets4/node57/with-indices→with-promises n)
           (define f
             (begin
               "fold-instance expanded code below. Initially called with:"
               '(fold-instance
                 (List (Listof Street51/index-type))
                 Void
                 (City50/index-type
                  (Promise City58/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'City46/with-indices-tag2)))
                  λ…)
                 (Street51/index-type
                  (Promise Street59/with-promises-type)
                  (λ (x)
                    (and (pair? x) (eq? (car x) 'Street47/with-indices-tag2)))
                  λ…)
                 (m-cities3/node52/index-type
                  (Promise m-cities3/node60/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-cities3/node48/with-indices-tag2)))
                  λ…)
                 (m-streets4/node53/index-type
                  (Promise m-streets4/node61/with-promises-type)
                  (λ (x)
                    (and (pair? x)
                         (eq? (car x) 'm-streets4/node49/with-indices-tag2)))
                  λ…))
               (λ ((val : (List (Listof Street51/index-type))) (acc : Void))
                 :
                 (values
                  (List (Listof (Promise Street59/with-promises-type)))
                  Void)
                 (let*-values (((temp221) (apply values val))
                               ((temp222 temp223)
                                ((λ ((val : (Listof Street51/index-type))
                                     (acc : Void))
                                   :
                                   (values
                                    (Listof
                                     (Promise Street59/with-promises-type))
                                    Void)
                                   (let ((f
                                          ((inst
                                            foldl
                                            Street51/index-type
                                            (Pairof
                                             (Listof
                                              (Promise
                                               Street59/with-promises-type))
                                             Void)
                                            Nothing
                                            Nothing)
                                           (λ ((x : Street51/index-type)
                                               (acc1
                                                :
                                                (Pairof
                                                 (Listof
                                                  (Promise
                                                   Street59/with-promises-type))
                                                 Void)))
                                             (let-values (((res res-acc)
                                                           ((ann
                                                             (λ ((tagged-index
                                                                  :
                                                                  Street51/index-type)
                                                                 (acc : Void))
                                                               :
                                                               (values
                                                                (Promise
                                                                 Street59/with-promises-type)
                                                                Void)
                                                               (values
                                                                (let ((successor-with-index
                                                                       (vector-ref
                                                                        Street75/database
                                                                        (cadr
                                                                         tagged-index))))
                                                                  (delay
                                                                   (Street55/with-indices→with-promises
                                                                    successor-with-index)))
                                                                acc))
                                                             (→
                                                              Street51/index-type
                                                              Void
                                                              (values
                                                               (Promise
                                                                Street59/with-promises-type)
                                                               Void)))
                                                            x
                                                            (cdr acc1))))
                                               (cons
                                                (cons res (car acc1))
                                                res-acc)))
                                           (cons '() acc)
                                           val)))
                                     (values (reverse (car f)) (cdr f))))
                                 temp221
                                 acc)))
                   (values (list temp222) temp223)))))
           (apply
            m-streets4/node65/make-with-promises
            (first-value (f (cdr n) (void))))))
       (delay
        (m-streets4/node57/with-indices→with-promises
         (vector-ref m-streets4/node77/database 0)))))))








         
         
         
         
         
         
         
         
         
         <graph-rich-return>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  typed/rackunit)
         
         #;<test-graph-rich-return>)]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]