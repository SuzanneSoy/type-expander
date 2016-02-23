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
          (define (strings→city [s : (Listof String)])
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
             (define-graph first-step
               #:definitions [<first-pass-type-expander>]
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
         (syntax-parse stx
           [(_ (~literal mapping))
            (template
             (U (first-step #:placeholder mapping/node)
                (tmpl-replace-in-type result-type
                                      [node (first-step #:placeholder node)]
                                      …)))]
           …))]

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
                              "../lib/low/multiassoc-syntax.rkt"
                              "rewrite-type.lp2.rkt"; debug
                              )
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
         
         
         
         
         
         #|(begin
           (define-graph
             first-step
             #:definitions [
 #;(define-type-expander (~> stx)
   (displayln stx)
   (displayln #'m-streets)
   (syntax-parse stx
     ((_ (~datum m-cities)) #'(U m-cities3/node (Listof City)))
     ((_ (~datum m-streets)) #'(U (first-step #:placeholder m-streets4/node)
                                  (Listof (first-step #:placeholder Street))))))


 

 (define-type-expander
      (~> stx)
      (syntax-parse stx
        ((_ (~literal m-cities))
         (template (U
            (first-step #:placeholder m-cities3/node)
            (tmpl-replace-in-type
             (Listof City)
             (City (first-step #:placeholder City))
             (Street (first-step #:placeholder Street))))))
        ((_ (~literal m-streets))
         (template (U
            (first-step #:placeholder m-streets4/node)
            (tmpl-replace-in-type
             (Listof Street)
             (City (first-step #:placeholder City))
             (Street (first-step #:placeholder Street))))))))

 ]
             (City
              (streets : (U m-streets4/node (Listof Street)))
              ((City1/simple-mapping (streets : (~> m-streets)
                                              #;(U (first-step #:placeholder m-streets4/node)
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
                    (City (m-streets s)))
                  (map strings→city cnames)))))
             (m-streets4/node
              (returned : (Listof Street))
              ((m-streets (snames : (Listof String)))
               (m-streets4/node
                (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
                  (map Street snames)))))))|#

(begin
   (define-graph
    first-step
    #:definitions
    ((define-type-expander
      (~> stx)
      (syntax-parse stx
        ((_ (~literal m-cities))
         (template (U
            (first-step #:placeholder m-cities3/node)
            (tmpl-replace-in-type
             (Listof City)
             (City (first-step #:placeholder City))
             (Street (first-step #:placeholder Street))))))
        ((_ (~literal m-streets))
         (template (U
            (first-step #:placeholder m-streets4/node)
            (tmpl-replace-in-type
             (Listof Street)
             (City (first-step #:placeholder City))
             (Street (first-step #:placeholder Street)))))))))
     #|
     (City [foo : Number] ((m1) (City 1)))
     (Street [foo : Number] ((m2) (Street 2)))
     (m-cities3/node [foo : Number] ((m3) (m-cities3/node 3)))
     (m-streets4/node [foo : Number] ((m4) (m-streets4/node 4)))
     |#

     ;; TODO: have a let-expander.
    (City
     (streets : (U m-streets4/node (Listof Street)))
     ((City1/simple-mapping (streets : #|(~> m-streets)|#
                                     (U (first-step #:placeholder m-streets4/node)
                                        (Listof (first-step #:placeholder Street)))
                                     )) (City streets)))
    (Street
     (sname : String)
     ((Street2/simple-mapping (sname : String)) (Street sname)))
    (m-cities3/node
     (returned : (Listof City))
     ((m-cities (cnames : (Listof (Listof String))))
      (m-cities3/node
       (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
         (define (strings→city (s : (Listof String))) (City (m-streets s))) ;;
         (map strings→city cnames)))))
    (m-streets4/node
     (returned : (Listof Street))
     ((m-streets (snames : (Listof String)))
      (m-streets4/node
       (let ((City City1/simple-mapping) (Street Street2/simple-mapping))
         (map Street snames)))))))
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         <graph-rich-return>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  typed/rackunit)
         
         ;<test-graph-rich-return>
         )]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]