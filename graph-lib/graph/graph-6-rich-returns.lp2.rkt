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
       (define-graph/rich-return name:id id-~>
         (~optional (~and #:debug debug))
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

@CHUNK[<graph-rich-return>
       (define-syntax/parse <signature>
         (define-temp-ids "first-step" name)
         (define-temp-ids "first-step-expander2" name)
         (define-temp-ids "~a/simple-mapping" (node …))
         (define-temp-ids "~a/node" (mapping …))
         (define-temp-ids "~a/extract/mapping" (node …))
         (define-temp-ids "~a/extract" (node …) #:first-base root)
         (define-temp-ids "~a/node-marker" (mapping …))
         (define-temp-ids "~a/from-first-pass" (node …))
         ;(define/with-syntax id-~> (datum->syntax #'name '~>))
         <inline-temp-nodes>
         (quasitemplate/debug debug
           (begin
             (define-graph first-step
               #:definitions [<first-pass-type-expander>]
               [node [field c (Let [id-~> first-step-expander2] field-type)] …
                [(node/simple-mapping [field c field-type] …)
                 ;<first-pass-field-type>] …)
                 (node field …)]] …
               [mapping/node [returned cm result-type]
                [(mapping [param cp param-type] …)
                 (mapping/node
                  (let ([node node/simple-mapping] …)
                    . body))]]
               …)
             ;; TODO: how to return something else than a node??
             ;; Possibility 1: add a #:main function to define-graph, which can
             ;; call (make-root).
             ;; Possibility 2: use the "(name node)" type outside as the return
             ;; type of functions
             (define-graph name
               #:definitions [<second-pass-type-expander>]
               [node [field c field-type] …
                [(node/extract/mapping [from : (first-step node)])
                 (node (<replace-in-instance> (get from field))
                       …)
                 …]]
               …)
             (begin
               (: node/extract (→ (first-step node) root))
               (define (node/extract from)
                 (meta-eval
                  (#,inline-temp-nodes/instance mapping/result-type
                                                #,(immutable-free-id-set)))))
             …
             (root/extract (first-step ???)) ;; choice based on #:root argument
             )))]

@chunk[<replace-in-instance>
       (tmpl-replace-in-instance
        (Let ~> second-step-marker-expander field-type)
        <second-pass-replace>)]

@chunk[<second-pass-type-expander>
       (define-type-expander (id-~> stx)
         (syntax-parse stx
           ;; TODO: should be ~literal
           [(_ (~datum mapping)) #'result-type] …
           ;; TODO: should fall-back to outer definition of ~>, if any?
           ))
       (define-type-expander (second-step-marker-expander stx)
         (syntax-parse stx
           ;; TODO: should be ~literal
           [(_ (~datum mapping)) #'mapping/node-marker] …
           ;; TODO: should fall-back to outer definition of ~>, if any?
           ))]

@chunk[<second-pass-replace>
       [mapping/node-marker
        <fully-replaced-mapping/result-type>
        (graph #:? mapping/node)
        (λ ([m : (first-graph mapping/node)])
          (get m val))]
       …]

The result of recursively inlining the temporary mapping nodes may be a
recursive type:

@chunk[<example-recursive-inlining>
       ;; TODO
       (m-a : (Listof (~> m-b)) …)
       (m-b : (Listof (~> m-a)) …)]

Since we prefer to not deal with infinite recursive structures (they could be
built using @tc[make-reader-graph], but this would not fit well with the rest of
our framework), we do not allow type cycles unless they go through a
user-defined node like @tc[a] or @tc[b] (by opposition to first-pass mapping
nodes like @tc[ma/node] or @tc[mb/node]).

The result type of inlining the temporary mapping nodes can be obtained by
inlining the types in the same way:

@CHUNK[<inline-temp-nodes>
       (define (inline-temp-nodes/type t seen)
         (quasitemplate
          (tmpl-replace-in-type (Let ~> second-step-marker-expander t)
            [mapping/node-marker
             (meta-eval
              (if (free-id-set-member? #,t #,seen)
                  (raise-syntax-error "Cycle in types!")
                  (#,inline-temp-nodes/type result-type
                                            #,(free-id-set-add t seen))))]
            …)))
       
       (define (inline-temp-nodes/instance t seen)
         (quasitemplate
          (tmpl-fold-instance (Let ~> second-step-marker-expander t)
            [mapping/node-marker
             (meta-eval
              (#,inline-temp-nodes/type result-type
                                        (free-id-set-add #,t #,seen)))
             (first-pass #:? mapping/node)
             (if (free-id-set-member? t seen)
                 (raise-syntax-error "Cycle in types!")
                 (inline-temp-nodes/instance result-type
                                             (free-id-set-add t seen)))]
            …
            [node/from-first-pass
             (name #:placeholder node) ; new type
             (first-pass #:? node)
             node] ;; call constructor
            …)))]


----------------------


As explained above, during the first pass, the field types
of nodes will allow placeholders for the temporary nodes
encapsulating the result types of mappings.

@chunk[<first-pass-type-expander>
       ;; TODO: to avoid conflicting definitions of ~>, we should either use
       ;; syntax-parameterize, or make a #:local-definitions
       (define-type-expander (id-~> stx)
         (syntax-parse stx
           [(_ (~datum mapping)) ;; TODO: should be ~literal
            (template
             (U (first-step #:placeholder mapping/node)
                (tmpl-replace-in-type result-type
                  [node (first-step #:placeholder node)]
                  …)))]
           …
           ;; TODO: should fall-back to outer definition of ~>, if any.
           ))
       (define-type-expander (first-step-expander2 stx)
         (syntax-parse stx
           [(_ (~datum mapping)) ;; TODO: should be ~literal
            #'(U mapping/node result-type)]
           …
           ;; TODO: should fall-back to outer definition of ~>, if any.
           )
         #;(U (first-step #:placeholder m-streets4/node)
              (Listof (first-step #:placeholder Street))))]

@; TODO: replace-in-type doesn't work wfell here, we need to define a
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
                              (submod "../lib/low.rkt" untyped)
                              "rewrite-type.lp2.rkt" #|debug|#
                              syntax/id-set)
                  (rename-in "../lib/low.rkt" [~> threading:~>])
                  "graph.lp2.rkt"
                  "get.lp2.rkt"
                  "../type-expander/type-expander.lp2.rkt"
                  "../type-expander/multi-id.lp2.rkt"
                  "structure.lp2.rkt" ; debug
                  "variant.lp2.rkt" ; debug
                  "fold-queues.lp2.rkt"; debug
                  "rewrite-type.lp2.rkt"; debug
                  "meta-struct.rkt"; debug
                  racket/stxparam
                  racket/splicing)
         (provide define-graph/rich-return); ~>)
         
         ;(define-syntax-parameter ~> (make-rename-transformer #'threading:~>))
         
         (require (for-syntax racket/pretty))
         
         <graph-rich-return>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  typed/rackunit)
         
         ;;<test-graph-rich-return>
         )]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]