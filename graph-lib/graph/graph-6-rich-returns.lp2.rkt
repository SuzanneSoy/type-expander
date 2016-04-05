#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Syntactic sugar for 
 @racket[graph]: rich return types}

@(table-of-contents)

@section{Introduction}

We build a wrapper around the @tc[graph] macro, which
allows defining mappings with rich return types, instead of
being forced to return a single node. For example, a mapping
can return a list of nodes, instead of having to push the
list operations up in the “caller” nodes.

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
two phases: first, we will associate a temporary
single-field node with each mapping, and use it as their
opaque return type. Then, a second pass will inline these
temporary nodes, and extract their constituents in-depth
until an actual user-specified node is reached.

Since this implementation also allows serveral mappings to
return the same node, the new signature separates the
mapping declarations from the node definitions:

@chunk[<signature>
       (define-graph/rich-return name:id id-~>
         (~optkw #:debug)
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

@tc[define-graph/rich-return] introduces a new
type-expander, @tc[id-~>], which is used as a special marker
to denote the return type of a mapping: @tc[(~> some-mapping)]
is expanded to the actual return type for @tc[some-mapping].
This notation is needed to facilitate the substitution of a
mapping's return type by a temporary node.

@tc[(~> m-streets)] which will be expanded to the return type of 
@tc[m-streets] (namely @tc[(Listof Street)]) in the final
graph type. For the first step, however, it will be expanded
to @tc[(U (gr #:placeholder m-streets/node) (Listof Street))].
Without this, passing the result of @tc[(m-streets s)] to 
@tc[City] would be impossible: the former is a placeholder
for the temporary node type which encapsulates the result of
@tc[m-streets], while the latter would normally expect a
plain list.

@CHUNK[<graph-rich-return>
       (define-syntax/parse <signature>
         (define/with-syntax (node* …) #'(node …))
         (define-temp-ids "~a/first-step" name)
         (define/with-syntax name/second-step ((make-syntax-introducer) #'name))
         (define/with-syntax (root-mapping/result-type . _) #'(result-type …))
         (define-temp-ids "first-step-expander2" name)
         (define-temp-ids "top1-accumulator-type" name)
         (define-temp-ids "~a/constructor-top2" (mapping …)
           #:first-base root-mapping)
         (define-temp-ids "~a/accumulator" (node …))
         (define-temp-ids "~a/top2-roots" (node …))
         (define-temp-ids "~a/next-idx" (node …))
         (define-temp-ids "~a/simple-mapping" (node …))
         (define-temp-ids "~a/node" (mapping …))
         (define-temp-ids "~a/extract/mapping" (node …))
         (define-temp-ids "~a/extract" (node …) #:first-base root)
         (define-temp-ids "~a/node-marker" (mapping …))
         (define-temp-ids "~a/node-marker2" (mapping …))
         (define-temp-ids "~a/from-first-pass" (node …))
         (define-temp-ids "second-step-~a/node-of-first" (mapping …))
         (define-temp-ids "second-step-~a-of-first" (node …))
         (define-temp-ids "~a/node-index" (mapping …))
         (define-temp-ids "~a/node-index-marker" (mapping …))
         (define-temp-ids "~a/node-index?" (mapping …))
         ;(define step2-introducer (make-syntax-introducer))
         ;(define/with-syntax id-~> (datum->syntax #'name '~>))
         ;(define/with-syntax introduced-~> (datum->syntax #'name '~>))
         (quasitemplate/debug debug
           (begin
             (define-graph name/first-step
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
             <step2>
             #|
             (begin
               (: node/extract (→ (first-step node) root))
               (define (node/extract from)
                 (meta-eval
                  (#,inline-temp-nodes/instance mapping/result-type
                                                #,(immutable-free-id-set)))))
             …
             (root/extract (first-step ???)) ;; choice based on #:root argument
             |#)))]

When declaring the nodes in the second step, @tc[~>] expands to the actual
result type of the user-provided mappings, for example @tc[(Listof Street)]:

@chunk[<second-step-~>-expander>
       (define-type-expander (~>-to-result-type stx)
         (syntax-parse stx
           ;; TODO: should be ~literal
           [(_ (~datum mapping)) #'result-type] …
           ;; TODO: should fall-back to outer definition of ~>, if any?
           ))]

The goal of these mappings is to inline the temporary nodes, and return a value
which does not refer to them anymore:


Where @tc[second-step-marker-expander] (in the input type
to @tc[replace-in-instance]) expands to the temporary marker
produced by the first step.

@chunk[<second-step-marker-expander>
       ;; TODO: should use Let or replace-in-type, instead of defining the node
       ;; globally like this.
       ;(define-type node (name/first-step node))
       ;…
       #|
       (define-type mapping/node-marker (U result-type
       (name/first-step node)))
       ;; TODO: shouldn't it be (name/first-step mapping/node) ?
       …
       |#
       (define-type mapping/node-marker
         (tmpl-replace-in-type result-type
           [mapping/node mapping/node-marker] ;;;;;;;TODO: test: I'm unsure here
           [node (name/second-step #:placeholder node)])
         #;(U (name/first-step mapping/node)
              (tmpl-replace-in-type result-type
                [mapping/node (name/first-step mapping/node)]
                [node (name/first-step node)])))
       …
       
       ;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TODO;^^;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define-type-expander (second-step-marker-expander stx)
         (syntax-parse stx
           ;; TODO: should be ~literal
           [(_ (~datum mapping)) #'mapping/node-marker] …
           ;; TODO: should fall-back to outer definition of ~>, if any?
           ))
       
       (define-type second-step-mapping/node-of-first
         (name/first-step mapping/node))
       …
       
       (define-type second-step-node-of-first
         (name/first-step node))
       …
       
       ;; TODO: we should take care here: inside result-type, node names get
       ;; bound to the identifier from the second graph, whereas semantically
       ;; they denote nodes from the first graph. Since they get rewritten as
       ;; second-step-node-of-first almost immediately it doesn't cause any
       ;; problem, but it's a nasty gotcha. Hoisintg this out of the
       ;; second graph definition should be enough. This probably happens in
       ;; another couple of places too.
       (define-type-expander (second-step-marker2-expander stx)
         (syntax-parse stx
           ;; TODO: should be ~literal
           [(_ (~datum mapping))
            (syntax-local-introduce
             #'(U second-step-mapping/node-of-first
                  result-type))]
           …
           ;; TODO: should fall-back to outer definition of ~>, if any?
           ))]

Replacing a marker node is as simple as extracting the
contents of its single field.

@subsection{Fully-inlined type}

The result of recursively inlining the temporary mapping nodes may be a
recursive type:

@chunk[<example-recursive-inlining>
       ;; TODO
       (m-a : (Listof (~> m-b)) …)
       (m-b : (Listof (~> m-a)) …)]

Since we prefer to not deal with the possible cyclic data
(that could be built using @tc[make-reader-graph]), we do
not allow type cycles unless they go through a user-defined
node like @tc[a] or @tc[b] (by opposition to first-pass
mapping nodes like @tc[ma/node] or @tc[mb/node]).

The result type of inlining the temporary mapping nodes can be obtained by
inlining the types in a way similar to what is done for the instance:

We replace (using the @tc[~>] syntax expander) the
occurrences of @tc[(~> some-mapping)] with a marker
identifier, so that it can be matched against by 
@tc[tmpl-replace-in-instance] and
@tc[tmpl-replace-in-type].

----------------------





@CHUNK[<step2>
       (define-graph name/second-step
         #:definitions [<second-step-~>-expander>
                        <second-step-marker-expander>
                        <inline-type>
                        <inline-instance>]
         [node [field c (Let [id-~> ~>-to-result-type] field-type)] …
          [(node/extract/mapping [from : (name/first-step node)])
           <inlined-node>]]
         …)
       
       <inline-type-top1>
       <inline-instance-top1-types>
       <inline-instance-top1>
       <inline-instance-top3>
       <inline-instance-top2>
       <define-multi-id>]

We create the inlined-node by inlining the temporary nodes
in all of its fields:

@chunk[<inlined-node>
       (node ((inline-instance* field-type
                                ()) (get from field))
             …)]

@subsection{Inlining instances}
To inline the temporary nodes in the instance, we use 
@tc[replace-in-instance], and call the inline-instance
recursively:

@CHUNK[<inline-instance>
       (define-syntax (inline-instance* stx)
         (syntax-parse stx
           [(_ i-ty seen)
            (define/with-syntax replt
              (replace-in-type #'(Let (id-~> second-step-marker2-expander)
                                      i-ty)
                               #'([node second-step-node-of-first]
                                  …)))
            #'(inline-instance replt seen)]))
       
       (define-syntax (inline-instance stx)
         (syntax-parse stx
           [(_ i-t (~and seen (:id (… …))))
            <inline-check-seen>
            (replace-in-instance #'i-t
              #'(<inline-instance-replacement>
                 <inline-instance-nodes>))]))]

@chunk[<inline-instance-replacement>
       [second-step-mapping/node-of-first                             ;; from
        (inline-type result-type (mapping/node . seen))               ;; to
        (name/first-step #:? mapping/node)                            ;; pred?
        (λ ([x : second-step-mapping/node-of-first])                  ;; fun
          ((inline-instance* result-type (mapping/node . seen))
           (get x returned)))]
       …]

@chunk[<inline-instance-nodes>
       [second-step-node-of-first             ;; node of first step   ;; from
        (name/second-step #:placeholder node) ;; new type             ;; to
        (name/first-step #:? node)                                    ;; pred?
        node/extract/mapping]                 ;; call mapping         ;; fun
       …]

@subsection{Inlining instances, at the top}

We need to inline the mapping nodes between the root mapping node and the first
layer of actual nodes. We do this in three steps:

@; TODO: example + test

@itemlist[
 @item{First, we replace the actual nodes with
  placeholders, which contain just an index, and aggregate
  these nodes in lists (one per node type)}
 @item{Then, we create the second-pass graph, using these
  nodes as the roots}
 @item{Finally, we replace the placeholders with the
  second-pass nodes returned by the graph.}]

@CHUNK[<inline-instance-top1-types>
       (define-constructor mapping/node-index
         #:private
         #:? mapping/node-index?
         Index)
       …
       (define-type mapping/node-index-marker mapping/node-index)
       …
       
       (define-type top1-accumulator-type
         (List (Pairof Index ;; max
                       (AListof mapping/node-index (name/first-step node)))
               …))
       
       (define-type-expander (second-step-marker2-top-expander stx)
         (syntax-parse stx
           [(_ (~datum mapping))
            (syntax-local-introduce
             #'(U second-step-mapping/node-of-first
                  result-type))]
           …
           ;; TODO: should fall-back to outer definition of ~>, if any?
           ))]

@CHUNK[<inline-instance-top1>
       (define-syntax (inline-instance-top1* stx)
         (syntax-parse stx
           [(_ i-ty seen)
            (define/with-syntax replt
              (replace-in-type #'(Let (id-~> second-step-marker2-top-expander)
                                      i-ty)
                               #'([node second-step-node-of-first]
                                  …)))
            #'(inline-instance-top1 replt seen)]))
       
       (define-syntax (inline-instance-top1 stx)
         (syntax-parse stx
           [(_ i-t (~and seen (:id (… …))))
            <inline-check-seen>
            ;(replace-in-instance #'i-t
            (fold-instance #'i-t
                           #'top1-accumulator-type
                           #'(<inline-instance-top1-replacement>
                              <inline-instance-top1-nodes>))]))]

@chunk[<inline-instance-top1-replacement>
       [second-step-mapping/node-of-first                             ;; from
        (inline-type-top1 result-type (mapping/node . seen))          ;; to
        (name/first-step #:? mapping/node)                            ;; pred?
        (λ ([x : second-step-mapping/node-of-first]                   ;; fun
            [acc : top1-accumulator-type])
          (% inlined new-acc
             = ((inline-instance-top1* result-type (mapping/node . seen))
                (get x returned)
                acc)
             in
             (values inlined new-acc)))]
       …]

@chunk[<inline-instance-top1-nodes>
       [second-step-node-of-first             ;; node of first step   ;; from
        mapping/node-index-marker             ;; new type             ;; to
        (name/first-step #:? node)                                    ;; pred?
        (λ ([x : second-step-node-of-first]   ;; record the old node  ;; fun
            [acc : top1-accumulator-type])
          : (values mapping/node-index-marker
                    top1-accumulator-type)
          (% ([node/next-idx . node/accumulator] …) = acc
             new-index = (mapping/node-index node/next-idx)
             in
             (values new-index
                     (let ([node/accumulator (cons (cons new-index x)
                                                   node/accumulator)]
                           [node/next-idx (assert (add1 node/next-idx) index?)])
                       (list (cons node/next-idx node/accumulator) …)))))]
       …]

@chunk[<inline-type-top1>
       (define-type-expander (inline-type-top1 stx)
         (syntax-parse stx
           [(_ i-t (~and seen (:id (… …))))
            <inline-check-seen>
            (replace-in-type #'(Let ([id-~> second-step-marker-expander]) i-t)
                             #'(<inline-type-top1-replacement>
                                <inline-type-top1-nodes>))]))]


@chunk[<inline-type-top1-replacement>
       [second-step-mapping/node-of-first                             ;; from
        (inline-type-top1 result-type (mapping/node . seen))]         ;; to
       …]

@chunk[<inline-type-top1-nodes>
       [node ;second-step-node-of-first ;; generated by the first pass
        mapping/node-index-marker] ;; new type
       …]

@chunk[<inline-instance-top2>
       (define (mapping/constructor-top2 [param cp param-type] …)
         (% <constructor-top2-body>))
       …]

@chunk[<constructor-top2-body>
       first-graph = (name/first-step #:root mapping/node param …)
       alists = (list (!each mapping '[0 . ()]) …)
       with-indices-top1 last-acc = ((inline-instance-top1* result-type ())
                                     (get first-graph returned)
                                     alists)
       ([node/next-idx . node/accumulator] …) = last-acc
       in
       (assert (= (length node/accumulator) node/next-idx))
       ;; Call the second step graph constructor:
       (% (node/top2-roots …)
          = (name/second-step
             #:roots [node (reverse (lists (cdrs node/accumulator)))] …)
          in
          ((replace-markers-top3 result-type
                                 node/top2-roots …)
           with-indices-top1))]

@chunk[<inline-instance-top3>
       (define-type-expander (inline-type-top3 stx)
         (syntax-parse stx
           [(_ i-ty)
            (replace-in-type #'(inline-type-top1 i-ty ())
                             #'([mapping/node-index-marker            ;; from
                                 (name/second-step node)]             ;; to
                                …))]))
       
       (define-syntax (replace-markers-top3 stx)
         (syntax-parse stx
           [(_ i-ty node/top2-roots …)
            (replace-in-instance #'(inline-type-top1 i-ty ())
              #'([mapping/node-index-marker                         ;; from
                  (name/second-step node)                           ;; to
                  mapping/node-index?                               ;; pred?
                  (λ ([idx : mapping/node-index])                   ;; fun
                    (vector-ref node/top2-roots
                                (constructor-values idx)))]
                 …))]))]

@subsection{The main graph macro}

@; TODO: move this to a separate file:
@chunk[<define-multi-id>
       (define-multi-id name
         #:type-expander <graph-type-expander>
         #:call (λ (stx)
                  (syntax-parse stx
                    ;; TODO: move this to a dot expander, so that writing
                    ;; g.a gives a constructor for the a node of g, and
                    ;; (g.a foo bar) or (let ((c .g.a)) (c foo bar)) both
                    ;; call it
                    [(_ #:λroot (~datum mapping))
                     #'root-mapping/constructor-top2]
                    …
                    [(_ #:root (~datum mapping) . rest)
                     (syntax/loc stx (mapping/constructor-top2 . rest))]
                    …
                    
                    #;[(_ #:roots [(~datum node) node/multi-rest] …)
                       (syntax/loc stx
                         (name/multi-constructor node/multi-rest …))]
                    ;; TODO: TR has issues with occurrence typing and promises,
                    ;; so we should wrap the nodes in a tag, which contains a
                    ;; promise, instead of the opposite (tag inside promise).
                    [(_ #:? (~datum node))
                     ;; TODO: implement node? properly here! FB case 107
                     (syntax/loc stx (name/second-step #:? node))]
                    …
                    [(_ . rest)
                     (syntax/loc stx (root-mapping/constructor-top2 . rest))]))
         #:id (λ (stx) #'root-mapping/constructor-top2))]

@chunk[<graph-type-expander>
       (λ (stx)
         (syntax-parse stx
           [:id #'(inline-type-top3 root-mapping/result-type)]
           [(_ (~datum mapping)) #'(inline-type-top3 result-type)]
           …
           [(_ . rest) #'(name/second-step . rest)]))]

@subsection{Inlining types}

The input type for the inlining of field @tc[streets] of the node @tc[City] is:

@chunk[<example-inline-input>
       (U m-street (Listof Street))]

Where @tc[m-street] is the @emph{with-promises} node type
of @tc[name/first-step], and @tc[Street] is the
@emph{with-promises} node type of @tc[name/first-step].

More generally, @tc[(~> some-mapping)] in the first pass is expanded to:

@chunk[<example-inline-input-2>
       (U (first-pass some-mapping)
          (tmpl-replace-in-type result-type
            [mapping/node (first-pass mapping/node)]
            [node (first-pass node)]))]

When inlining, we want to first inline the 
@tc[some-mapping] node, if it is present, and in all cases
drill down the result-type in both cases (either we just
inlined it, or it was already there). It would be nice to
avoid duplicating the code for inlining inside the
result-type, as the code would grow exponentially with the
number of mappings encountered along the path otherwise.

We would need to call the replace-in-instance a second time
on the result. The generated code would have a shape like this:

@chunk[<example-generated-inline>
       (λ ([v : (V (first-pass some-mapping)
                   (tmpl-replace-in-type result-type
                     [mapping/node (first-pass mapping/node)]
                     [node (first-pass node)]))])
         ((λ ([v : (tmpl-replace-in-type result-type
                     [mapping/node (first-pass mapping/node)]
                     [node (first-pass node)])])
            …)
          (if ((first-pass #:? some-mapping) v)
              <inline-v>
              v)))]

This would require some specific support from rewrite-type.

We could have a node with the following type:

@chunk[|<example (V (~> 1) (~> 2) …)>|
       (define-graph/rich-return grr
         ([Node [field : (V (~> m-1) (~> m-2) #:or (~> m-3))]]))]

where @tc[m-1], @tc[m-2] and @tc[m-3] have different return
types, but @tc[m-1] and @tc[m-2] are constructors or tagged
structures. If we expand this a bit, we see the following type:

@chunk[|<example (V (~> 1) (~> 2) …) expanded>|
       (V (V (first-pass m-1)
             some-constructor-1)
          (V (first-pass m-2)
             some-constructor-2)
          #:or (U (first-pass m-3)
                  some-abritrary-type-3))]

Which is equivalent to:

@chunk[|<example (V (~> 1) (~> 2) …) merged>|
       (V (first-pass m-1)
          some-constructor-1
          (first-pass m-2)
          some-constructor-2
          (first-pass m-3)
          #:or some-abritrary-type-3)]

The generated code would roughly be (possibly without
merging the node + return-type pairs):

@chunk[|<example (V (~> 1) (~> 2) …) generated >|
       (λ ([v : (V (first-pass m-1)
                   some-constructor-1
                   (first-pass m-2)
                   some-constructor-2
                   (first-pass m-3)
                   #:or some-abritrary-type-3)])
         (cond
           [(or ((first-pass #:? m-1) v) (some-constructor-1? v))
            ((λ ([v : some-constructor-1]) …)
             (if ((first-pass #:? m-1) v)
                 <inline-v>
                 v))]
           [(or ((first-pass #:? m-2) v) (some-constructor-2? v))
            ((λ ([v : some-constructor-2]) …)
             (if ((first-pass #:? m-2) v)
                 <inline-v>
                 v))]
           [else
            ((λ ([v : some-abritrary-type-3]) …)
             (if ((first-pass #:? m-3) v)
                 <inline-v>
                 v))]))]

Detecting whether we can safely use variants for the first
two cases (@tc[m-1] and @tc[m-2]) requires knowing if the 
@tc[~>] was in a variant position or in the @tc[#:or]
position, or to change the user syntax a bit.

As of 2016-03-18, however, rewrite-type doesn't support yet
variants, so we will use a temporary inefficient solution,
which does not allow variants of (~> …).

----

@chunk[<inline-type>
       (define-type-expander (inline-type stx)
         (syntax-parse stx
           [(_ i-t (~and seen (:id (… …))))
            <inline-check-seen>
            (replace-in-type #'(Let ([id-~> second-step-marker-expander]) i-t)
                             #'(<inline-type-replacement>
                                <inline-type-nodes>))]))]


@chunk[<inline-type-replacement>
       [second-step-mapping/node-of-first                             ;; from
        (inline-type result-type (mapping/node . seen))]              ;; to
       …]

@chunk[<inline-type-nodes>
       [node ;second-step-node-of-first ;; generated by the first pass
        (name/second-step #:placeholder node)] ;; new type
       …]

We detect the possibility of unbounded recursion when
inlining nodes by remembering the ones alreday traversed.

@chunk[<inline-check-seen>
       (let ([seen-list (syntax->list #'seen)])
         (when (and (not (null? seen-list))
                    (member (car seen-list) (cdr seen-list) free-identifier=?))
           (raise-syntax-error 'define-graph/rich-returns
                               (~a "Cycles in types are not allowed."
                                   " The following types were already inlined: "
                                   (syntax->datum #'seen)
                                   ", but " #'t " appeared a second time.")
                               #'t)))]

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
             (U (name/first-step #:placeholder mapping/node)
                (tmpl-replace-in-type result-type
                  [node (name/first-step #:placeholder node)]
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
         #;(U (name/first-step #:placeholder m-streets4/node)
              (Listof (name/first-step #:placeholder Street))))]

@; TODO: replace-in-type doesn't work wfell here, we need to define a
@; type-expander.
@chunk[<first-pass-field-type>
       (tmpl-replace-in-type field-type
         [(~> mapping) (U mapping/node result-type)] …)]

@section{Conclusion}

@CHUNK[<super-graph-rich-return>
       (define-syntax (super-define-graph/rich-return stx)
         (syntax-case stx ()
           [(_ name . rest)
            (with-syntax ([(b (d (dgi n) . r) (dgi2 n2))
                           #`(begin
                               (define-syntax-rule (dg1 name)
                                 (define-graph/rich-return name
                                   #,(replace-context stx #'~>)
                                   . rest))
                               (dg1 name))])
              #'(b (d (dgX n) . r) (dgX n2)))]))]

@chunk[<module-main>
       (module main typed/racket
         (provide define-graph/rich-return)
         
         (require (for-syntax syntax/parse
                              syntax/parse/experimental/template
                              racket/syntax
                              (submod phc-toolkit untyped)
                              "rewrite-type.lp2.rkt"
                              racket/format)
                  phc-toolkit
                  "graph.lp2.rkt"
                  "get.lp2.rkt"
                  "../type-expander/type-expander.lp2.rkt"
                  "../type-expander/multi-id.lp2.rkt"
                  "adt.lp2.rkt"
                  "rewrite-type.lp2.rkt")
         
         <graph-rich-return>)]

@chunk[<module-wrapper>
       (module wrapper typed/racket
         (provide (rename-out [super-define-graph/rich-return define-graph]))
         
         (require (submod ".." main)
                  (for-syntax syntax/strip-context))
         
         <super-graph-rich-return>)]

@chunk[<module-test-syntax>
       (module test-syntax racket
         (provide tests)
         (define tests
           (quote-syntax
            (begin
              <test-graph-rich-return>))))]

@chunk[<*>
       (begin
         <module-main>
         <module-wrapper>
         
         (require 'wrapper)
         (provide (all-from-out 'wrapper))
         
         <module-test-syntax>)]