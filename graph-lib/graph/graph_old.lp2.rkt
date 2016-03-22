#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Graph implementation}

This module provides (a simplified form of) recursive algebraic data structures,
with the ability to handle the structure as a collection of nodes, and process
them all in a way similar to what @tc[map] provides. Traditionally, immutable
data structures can't form cycles, but can easily be traversed to reach all
nodes. Conversely, iterating over a cyclic data structure (created via lazy
evaluation or thunks) is difficult if at all possible.

More formally, this module offers fold operations on heterogeneous, richly typed
graphs.

@(table-of-contents)

@section{Notes on complex transform result types}

We wish at one point to support complex result types for the transforms, instead
of only allowing a single node type.

We have to impose a constraint: do not have a cycle inside the transform's
result that doesn't go through a node, since we break cycles by replacing nodes
with a promise. The safest way to satisfy that constraint is to enforce the
absence of loops at the type level.

We would then inline the called transform's results, breaking the cycles by
replacing nodes with a thunk that returns the desired node. That thunk will be
wrapped into a Promise that calls it, so that typed/racket's occurrence typing
is happy, but we don't rely on the memoization semantics.

@subsection{Compile-time handling of complex transform result types}

During macro-expansion, we generate procedures that process nodes found in
transforms' results, by inlining the results of called transforms. If we find a
@tc[transform/link-request] type in some place we don't know how to rewrite
(like a function type, for example), we throw an error. Similarly, if we
encounter a cycle in the type that does not go through a node type, we throw an
error.

These procedures will help generate code to make a facade node from the
incomplete one. When inlining results from called transforms, they will request
other incomplete nodes from the database.

@subsection{Two-step graph creation}

Writing a graph-generation macro that allows complex return types for transforms
seems difficult, and it would be easier to write a simple graph-generation
macro, that only accepts transforms with return a single node type. We could
build on top of that a more flexible macro, that would first generate a graph
where each transform's result is wrapped in an ad-hoc single-field node. Then,
we would automatically generate a second graph transformation that produces the
desired nodes from that graph.

Example: transform @tc[t1] takes a list of numbers as input, and produces a list
of either calls to transform @tc[t2] or nodes @tc[ni] as output. The @tc[t2]
transform generates a pair of nodes @tc[(ni [x Number])] and
@tc[(nj [y String])].

The user would describe the graph like this:

@chunk[<example-1674389-2>
       (make-graph ([root (Listof (U ni (Pairof ni nj)))]
                    [ni [x Number]]
                    [nj [y String]])
                   [(t1 [ln : (Listof Number)] : (Listof (U ni t2))
                        (map (λ (x) (if (even? x)
                                        (t2 x)
                                        (ni x)))
                             ln))]
                   [(t2 [n : Number] : (Pairof ni nj)
                        (cons (ni n) (nj (format "~a" n))))])]

In the above, the result type of @tc[t1] has to be @tc[(Listof (U ni t2))]
instead of @tc[(Listof (U ni (Pairof ni nj)))], because otherwise we can't
easily automatically infer that @tc[(Pairof ni nj)] was actually @tc[t2],
without looking at the body of the transform. In a more advanced version, we
could substitute every @tc[result-type] found in another transform's
@tc[result-type] by @tc[(U result-type transform/link-request)], however that
would likely produce spurious cycles that do not go through a node, so it's
probably best to make things explicit, and let the user write @tc[U].

@chunk[<example-1674389>
       (graph ([r-t1 [result (Listof (U ni t2))]]
               [r-t2 [result (Pairof ni nj)]])
              [(t1 [ln : (Listof Number)] : r-t1
                   (r-t1 (map (λ (x) (if (even? x)
                                         (t2 x)
                                         (ni x)))
                              ln)))]
              [(t2 [n : Number] : r-t2
                   (r-t2 (cons (ni n)
                               (nj (format "~a" n)))))])]

Then use this graph transform:

@chunk[<example-1674389-2>
       (make-graph ([root [result (Listof (Pairof ni nj))]]
                    [ni [x Number]]
                    [nj [y String]])
                   [(r-t1→root [t1 : r-t1]) : root
                    (root (map (λ (v)
                                 (match v
                                   [(? list?) (r-t2-result v)]
                                   [(ni _) v]))
                               (r-t1-result t1)))])]

@subsection{Many to one transforms}

This example covers one to many transforms. What about many to one transforms?
The macro we are building allows generating graphs, but does not care about the
input. In the case were transforming a graph of @tc[house]s, @tc[street]s and a
@tc[city], and we want to condense all the @tc[house]s on one side of each
@tc[street] to a @tc[suburb], we would write a transform @tc[t1] for@tc[street]
which passes the whole list of @tc[house]s to a transform @tc[t2]. The @tc[t2]
transform would create a @tc[suburb] from those, without calling a transform for
each @tc[house].

@subsection{Implicit rule names}

In order to allow implicit rule names, when there's only one rule with the
desired result node, we can use the node's name as the transform name. We should
think about naming conflicts: when calling @tc[n], should it insert a link
request for the transform, or should it create an incomplete node?

@subsection[#:tag "graph|complex-transforms-return-type-conclusion"]{Conclusion}

With this approach, we can write the graph creation macro with the guaranty
that the result of a transform always is exactly one node type. More complex
transform result types can be decomposed into to two passes.

A downside is that we can't inspect the result of a call to another transform,
since it's not actually calling it, and we're only getting an opaque link
request back. We couldn't call the other transform anyway, because it could half
of the time return a value immediately, and half of the time call us back (with
the same arguments), causing an infinite loop. For that, we could declare some
#:helper transforms, that get called immediately (but if they run into an
infinite loop it's not our fault).

@section{Comparison with @racket[make-placeholder] and
 @racket[make-reader-graph]}

Comparison of this approach with @tc[make-placeholder] and
@tc[make-reader-graph]:

@itemlist[
 @item{They don't guarantee at compile-time that you'll fill in all
  placeholders. We could use @racket[make-placeholder] and
  @racket[make-reader-graph] wrapped inside a macro that makes sure that all
  placeholders are filled (same approach as we have).}
 @item{I don't think you can iterate over all the nodes or over the nodes of a
  specific type, and @racket[make-placeholder] isn't typed (yet) anyway I
  guess).}]

@section{Constructor}

Here is an overview of the architecture of the graph constructor:

@itemlist[
 @item{We first save the parameter types in the old context, because we later
  shadow the node names, and the parameters should refer to the old types.
  Depending on how we write the rest, this might not be necessary though, since
  it is possible we need to write @racket[(og node)] to refer to nodes types
  from the old graph @racket[og].}
 @item{We then define the node names as constructors for incomplete types —
  which means that they can contain link requests for the results other
  transforms}
 @item{We define data structures representing link requests. Each link request
  encapsulates a thunk that performs the transform's work when called, as well
  as the name of the transform and its arguments, used to detect when we have
  two identical link requests (which can be due to cycles in the resulting
  graph, for example).}
 @item{We then define the transforms as procedures that return a link request.}]

@chunk[<make-graph-constructor>
       (define-syntax/parse
         (make-graph-constructor ([node (field:id field-type:expr) ...] ...)
                                 [transform:id (param:id param-type:expr) ...
                                  (~literal :) result-type:id
                                  body ...]
                                 ...)
         <stx-transform/link-request>
         <stx-make-graph-database>
         <stx-node/incomplete>
         <stx-param-type/old>
         <stx-transform/result-node/extract-link-requests>
         <stx-transform/link-request→incomplete>
         #`(let ()
             <param-type/old>
             (let ()
               <define-incomplete-types>
               <define-make-link-requests>
               <transform/link-request→incomplete>
               <define-transforms>
               <make-graph-database>
               make-graph-database)))]

@chunk[<test-make-graph-constructor>
       (define make-g (make-graph-constructor
                       ([ma (fav String) (faa ma) (fab mb)]
                        [mb (fbv String) (fba ma)])
                       [transform-a (s String) : ma
                        (ma s
                            (transform-a s)
                            (transform-b "b"))]
                       [transform-b (s String) : mb
                        (mb s
                            (transform-a s))]))
       (make-g "root-arg")]

@subsection{Saving parameter types in old context}

@chunk[<stx-param-type/old>
       (define/with-syntax ((param-type/old ...) ...)
         (stx-map (λ (ps)
                    (with-syntax ([(t sps ...) ps])
                      (format-temp-ids "~a/~a/memorized-type" #'t #'(sps ...))))
                  #'((transform param ...) ...)))]

@chunk[<param-type/old>
       (define-type param-type/old param-type)
       ...
       ...]

@subsection{Incomplete nodes}

When a transform returns an object, it is incomplete (it potentially contains
link requests instead of actual references to the nodes).

We prepare some template variables. The first is the name of the tagged variant
representing an incomplete node:

@chunk[<stx-node/incomplete>
       (define/with-syntax (node/incomplete ...)
         (format-temp-ids "~a/incomplete" #'(node ...)))]

Then, we build a reverse map, which from a node type obtains all the transforms
returning that node type. More specifically, we are interested in the
transform's link request type.

@chunk[<stx-node/incomplete>
       (define/with-syntax ((node/link-request-types ...) ...)
         (for/list ([x (in-syntax #'(node ...))])
           (multiassoc-syntax x
                              #'([result-type . transform/link-request] ...))))]

The third template variable we define maps transforms to the incomplete type for
their returned node.

@chunk[<stx-node/incomplete>
       (define/with-syntax (transform/result-node/incomplete ...)
         (for/list ([x (in-syntax #'(result-type ...))])
           (assoc-syntax x #'([node . node/incomplete] ...))))]

@CHUNK[<define-incomplete-types>
       (define-type node (U node/link-request-types ...)
         #:omit-define-syntaxes)
       ...
       (define-tagged node/incomplete [field field-type] ...)
       ...
       (define-multi-id node
         #:match-expander-id node/incomplete
         #:call-id node/incomplete)
       ...]

@subsection{Link requests for nodes}

When a transform wants to produce a reference to the result of another transform
of some data, it generates instead a link request, which encapsulates the
desired transform and arguments, without actually performing it.

@chunk[<stx-transform/link-request>
       (define/with-syntax (transform/link-request ...)
         (format-temp-ids "~a/link-request" #'(transform ...)))]

Due to an issue with @tc[typed/racket] (@tc[struct]s aren't properly declared
inside a @tc[let]), we need to pre-declare the @tc[transform/link-request]
@tc[struct]. Since the call to make-graph could itself be inside a @tc[let], we
need to pre-declare it in this file, instead of declaring it at the top of the
macro.

We're making the structure transparent for easier debugging, but at the time of
writing this, it needs not be.

@chunk[<pre-declare-transform/link-request>
       (struct (TKey)
         transform/link-request-pre-declared
         ([key : TKey])
         #:transparent)]

@chunk[<define-make-link-requests>
       (define-type transform/link-request
         (transform/link-request-pre-declared
          (List 'transform
                param-type/old ...)))
       ...]

@subsection{Transforms}

@chunk[<stx-transform/link-request→incomplete>
       (define/with-syntax (transform/link-request→incomplete ...)
         (format-temp-ids "~a/link-request→incomplete" #'(transform ...)))]

@chunk[<transform/link-request→incomplete>
       (begin
         (: transform/link-request→incomplete
            (→ param-type/old ... transform/result-node/incomplete))
         (define (transform/link-request→incomplete param ...)
           body ...))
       ...]

@chunk[<define-transforms>
       (begin
         (: transform
            (→ param-type/old ...  transform/link-request))
         (define (transform param ...)
           ((inst transform/link-request-pre-declared
                  (List 'transform
                        param-type/old ...))
            (list 'transform param ...))))
       ...]

@section{Queue}

@chunk[<stx-make-graph-database>
       (define/with-syntax (root-transform . _) #'(transform ...))
       (define/with-syntax ((root-transform/param-type ...) . _)
         #'((param-type ...) ...))
       (define/with-syntax ((root-transform/param ...) . _)
         #'((param ...) ...))
       (define/with-syntax (transform/transformed ...)
         (format-temp-ids "~a/transformed" #'(transform ...)))
       (define/with-syntax (root-transform/link-request . _)
         #'(transform/link-request ...))
       (define/with-syntax recursive-call
         #'(process-queue pending-requests
                          processed-requests
                          transform/transformed ...))
       (define/with-syntax (node/extract-link-requests ...)
         (format-temp-ids "~a/extract-link-requests" #'(node ...)))
       <fold-type-clauses>
       <fold-type-stx>
       <stx-extract-link-requests>]

To build the graph database, we take the parameters for the root transform, and
return lists incomplete nodes (one for each transform).

The parameters for the root transform, addition to the transform's name, form
the first link request. To fulfil this link request and the ones found later,
we call the desired transform which returns an incomplete node. We extract any
link requests found in that incomplete node, and queue them. The incomplete node
itself is added to the appropriate list, to be returned once the queue has been
fully processed.

@CHUNK[<make-graph-database>
       (: make-graph-database
          (→ root-transform/param-type ...
             (List (Listof transform/result-node/incomplete) ...)))]

The @tc[make-graph-database] function consists mainly in the process-queue
function, which takes a queue for each transform, and a list of
already-processed incomplete nodes for each transform, and returns these lists,
once all queues are empty.

@CHUNK[<make-graph-database>
       (define (make-graph-database root-transform/param ...)
         (: process-queue (→ (Setof (U transform/link-request ...))
                             (Setof (U transform/link-request ...))
                             (Listof transform/result-node/incomplete)
                             ...
                             (List (Listof transform/result-node/incomplete)
                                   ...)))
         (define (process-queue pending-requests
                                processed-requests
                                transform/transformed
                                ...)
           <define-extract-link-requests> ;; TODO: Can probably be moved out.
           <process-queue-body>)
         
         <process-queue-initial-call>)]

The @tc[process-queue] function is initially called with empty lists for all
queues and all result lists, except for the root transform's queue, which
contains the initial link request.

@CHUNK[<process-queue-initial-call>
       (process-queue (set (root-transform root-transform/param ...))
                      (set)
                      (begin 'transform/transformed '())
                      ...)]

Process-queue is a standard queue handler using sets.

@CHUNK[<process-queue-body>
       (if (set-empty? pending-requests)
           (list transform/transformed ...)
           (let* ([request (set-first pending-requests)]
                  [pending-requests (set-rest pending-requests)]
                  [processed-requests (set-add processed-requests request)]
                  [tag (car (transform/link-request-pre-declared-key request))])
             <process-queue-body-tags>))]

To process each link request, we first match on its type, and once we found it,
we call the result thunk, extract any link requests contained within, and add
those to the queue.

@CHUNK[<process-queue-body-tags>
       (cond
         [(eq? tag 'transform)
          (let* ([transformed
                  : transform/result-node/incomplete
                  (apply transform/link-request→incomplete
                         (cdr (transform/link-request-pre-declared-key
                               request)))]
                 [transform/transformed
                  (cons transformed transform/transformed)]
                 [extracted
                  (list->set
                   (transform/result-node/extract-link-requests transformed))]
                 [pending-requests
                  (set-union pending-requests
                             (set-subtract extracted processed-requests))])
            recursive-call)]
         ...)]

@subsection[#:tag "graph|TODO3"]{TODO}

We need to traverse the @tc[transformed] node (which is an incomplete node),
and find the link requests within. These link requests will be added to the
corresponding @tc[pending-requests] queue. Below is the body of a for-syntax
function that transforms a type with link-requests into the @tc[match] patterns
that will be used at run-time to traverse the incomplete node. In most cases,
there is only one pattern, but the @tc[U] requires one for each possibility.

When we encounter a link request, we prepend it to the corresponding queue.
For the type @tc[(List Number n/link-request)], the function will look like
this:

@chunk[<fold-type-match-example>
       (match transformed
         [(list a b)
          (match a [a2 a2])
          (match b [(and t
                         (transform/link-request-pre-declared
                          (cons 'transform1 _)))
                    (set! pending-requests
                          (cons t pending-requests))])])]

@subsubsection{Match clauses}

We first transform the type into the different match clauses. For that, we
define the @tc[fold-type-clauses] function, which takes the identifier to
destructure at run-time, and its type. The function returns a list of clauses.

@chunk[<fold-type-clauses>
       (define (fold-type-clauses val t)
         (syntax-parse t
           <fold-type-clauses-body>))]

When a link request is found in the type, we produce the corresponding match
clause, which body prepends the request to the queue of pending requests. For
now we use @racket[set!] to prepend the request, but it would be cleaner to use
recursion. We wouldn't even need to flatten the pending-requests list, because
it could be a tree instead of a flat list, since we only need to add to it and
later pop elements.

TODO: we currently ignore potential hiding of identifiers due to type variables
bound by Rec, for example. This is a case where having a fold-type function
provided by the type-expander library would be interesting.

@CHUNK[<fold-type-clauses-body>
       [x:id
        #:when (ormap (curry free-identifier=? #'x)
                      (syntax->list #'(node/incomplete ...)))
        (define/with-syntax (this-field-type ...)
          (assoc-syntax #'x #'((node/incomplete field-type ...) ...)))
        
        (define/with-syntax (tmp ...)
          (generate-temporaries #'(this-field-type ...)))
        #`([(x tmp ...)
            (append #,@(stx-map fold-type
                                #'(tmp ...)
                                #'(this-field-type ...)))])]]

@CHUNK[<fold-type-clauses-body>
       [x:id
        #:when (ormap (curry free-identifier=? #'x)
                      (syntax->list #'(node ...)))
        #`([(and t (transform/link-request-pre-declared (cons 'transform _)))
            (cons (ann t transform/link-request) '())]
           ...)]]

We handle fixed-length lists by calling @tc[fold-type] on each element type.

@CHUNK[<fold-type-clauses-body>
       [((~literal List) a ...)
        (define/with-syntax (tmp ...) (generate-temporaries #'(a ...)))
        #`([(list tmp ...)
            (append #,@(stx-map fold-type #'(tmp ...) #'(a ...)))])]]

We iterate variable-length lists at run-time.

@CHUNK[<fold-type-clauses-body>
       [((~literal Listof) a)
        #`([(list tmp (... ...))
            (append-map (λ (tmp1) #,(fold-type #'tmp1 #'a))
                        tmp)])]]

Pairs and vectors are handled similarly:

@CHUNK[<fold-type-clauses-body>
       [((~literal Pairof) a b)
        #`([(cons tmpa tmpb)
            (list #,(fold-type #'tmpa #'a)
                  #,(fold-type #'tmpb #'b))])]]

@CHUNK[<fold-type-clauses-body>
       [((~literal Vectorof) a)
        #'([(vector tmp (... ...))
            (append-map (λ (tmp1) #,(fold-type #'tmp1 #'a))
                        tmp)])]]

For unions, we return several clauses, obtained via a recursive call to
@tc[fold-type-clauses].

@CHUNK[<fold-type-clauses-body>
       [((~literal U) a ...)
        #`(#,@(stx-map fold-type-clauses val #'(a ...)))]]

We handle other cases by leaving them as-is, but we still check that they don't
contain a reference to a node type, because we would otherwise leave the
link-request there.

And the fourth maps transforms to the link-requests extraction procedure for
their returned node.

@chunk[<stx-transform/result-node/extract-link-requests>
       (define/with-syntax (transform/result-node/extract-link-requests ...)
         (for/list ([x (in-syntax #'(result-type ...))])
           (assoc-syntax x #'([node . node/extract-link-requests] ...))))]

The last case is when we encounter an unknown type. We assume that it does not
contain any link-requests and therefore return an empty list.

@CHUNK[<fold-type-clauses-body>
       [x:id
        #`([_ '()])]]

@subsubsection{Folding the type: extracting link requests}

The for-syntax function @tc[fold-type] generates code that uses @tc[match] to
extract the @tc[link-request]s from an incomplete node (or part of it) with type
@tc[t]. The match clauses are those returned by @tc[fold-type-clauses] defined
above.

@CHUNK[<fold-type-stx>
       (define (fold-type val t)
         #`(begin
             (match #,val #,@(fold-type-clauses val t))))]

@subsubsection{Fold function for each incomplete node}

For each node type, we wish to declare a function that extracts link requests
from the incomplete type. We should work on the expanded type.

@chunk[<stx-extract-link-requests>
       (define-template-metafunction (fold-type-tmpl stx)
         (syntax-case stx () [(_ val t) (fold-type #'val #'t)]))]
@CHUNK[<define-extract-link-requests>
       #,@(for/list ([name (in-syntax #'(node/extract-link-requests ...))]
                     [val-type (in-syntax #'(node/incomplete ...))]
                     [field-types (in-syntax #'((field-type ...) ...))])
            #`(define (#,name [val : #,val-type])
                : (Listof (U transform/link-request ...))
                #,(fold-type #'val val-type)))]

@subsubsection[#:tag "graph|TODO1"]{TODO}

Later, we will replace link requests with thunks returning the desired node,
wrapped in a promise in order to please occurrence typing. Below is the body of
the for-syntax function that transforms a type with link-requests into a type
with actual nodes. It's probably not useful, because we obtain the same result
with scopes.

@CHUNK[<fold-type-cases>
       [x:id
        #:when
        (ormap (curry free-identifier=? #'x)
               (syntax->list #'(node/link-request ...)))
        #`(Promise (→ #,(assoc-syntax #'x #'((node/link-request . node) ...))))]
       [((~literal List) a ...) #`(List #,@(stx-map fold-type #'(a ...)))]
       [((~literal Listof) a) #`(Listof #,@(stx-map fold-type #'(a ...)))]
       [((~literal Pairof) a b) #`(Pairof #,(fold-type #'a) #,(fold-type #'b))]
       [((~literal Vectorof) a) #'(Vectorof #,(fold-type #'a))]
       [((~literal U) a ...) #'(U #,(stx-map fold-type #'(a ...)))]]

@section{@racket[incomplete] type-expander}

We define a @tc[type-expander] @tc[(incomplete n)] that returns the incomplete
node type for the node type @tc[n]. This type-expander allows the user to refer
to the incomplete type of the node in the body of a transform, if annotations
are needed for a value containing such a node.

@chunk[<outermost-incomplete>
       (define-type-expander (incomplete stx)
         (syntax-case stx ()
           [(_ n)
            (raise-syntax-error
             'incomplete
             (format "Type doesn't have an incomplete counterpart: ~a"
                     (syntax->datum #'n))
             #'n)]))]

@chunk[<save-outer-incomplete>
       (define-type-expander (outer-incomplete stx)
         (syntax-case stx () [(_ n) #'(incomplete n)]))]

@chunk[<incomplete>
       (let ()
         <save-outer-incomplete>
         (let ()
           (define-type node
             (tagged node [field (Promise field-type)] ...))
           ...
           
           (define-type node/incomplete
             ;; TODO: substitute link-requests here
             (tagged node [field (Promise field-type)] ...))
           
           (define-type-expander (incomplete stx)
             (syntax-parse stx ()
               [(_ (~litral node))  #'node/incomplete]
               [_ #'(outer-incomplete n)]))
           <body>))]

@section{Transforming @racket[incomplete] nodes into complete ones}

@subsection{Initial version}

We will start with a very simple traversal function, that will just substitute
link requests immediately in the fields of a node.

@chunk[<substitute-link-requests>
       (define (substitute-link-requests v)
         (match v
           [(node/incomplete field ...)
            (node <link-request→promise> ...)]
           ...))]

@chunk[<link-request→promise>
       (match field
         [(transform/link-request key _) (transform/key→promise key)] ;; TODO
         ...)]

@chunk[<transform/key→promise>
       ]

@subsection{More complex attempt}

We know for sure that all references to future nodes are actually incomplete
ones, but we have no guarantee about the contents of the fields of a node. Since
they may contain a mix of link requests and primitives (via a @tc[U] type for
example), and may contain lists of nodes etc. we need to traverse them at
run-time, in order to find and replace references to link requests.

However, if we were to write this as a simple recursive function, we wouldn't be
able to express its type without knowing anything about the node's type:

@chunk[<attempt-at-typing-traverse>
       (case→ (→ node/link-request node) ...
              (→ (Pairof may-contain-link-request
                         may-contain-link-request)
                 (Pairof doesnt-contain-link-request
                         doesnt-contain-link-request)))]

Writing the @tc[may-contain-link-request] and @tc[doesnt-contain-link-request]
as functions, while expressing the contraint that the output is the same type as
the input — except for the link requests that turned into nodes, would be
impossible in typed/racket. I suppose that with GADTs one could write such a
type.

Instead, we will, during macro-expansion, traverse the type, and generate
conversion procedures accordingly.

@chunk[<fold-type-cases2>
       [(~literal node/link-request) #''link-request]
       ...
       [((~literal List) a ...) #'(List #,@(stx-map fold-type #'(a ...)))]
       [((~literal Listof) a) #''Listof]
       [((~literal Pairof) a) #''Pairof]
       [((~literal Vectorof) a) #''Vectorof]
       [((~literal U) a ...) #''U]]

@chunk[<traverse-list-type>
       (→ (List a ...) (List replaced-a ...))]

@chunk[<traverse-list-code>
       [(list? v) (map traverse-list v)]
       [(pair? v) (cons (traverse-list (car v))
                        (traverse-list (cdr v)))]
       [(vector? v) ]]

@subsection{Unions}

Unions are difficult to handle: At one extreme, we confuse two different types
like @tc[(Listof Number)] and @tc[(Listof String)], by using just the @tc[list?]
predicate. On the other end of the spectrum, we try to distinguish them with
@tc[typed/racket]'s @tc[make-predicate], which doesn't work in all cases.

Handling this in the best way possible is out of the scope of this project, so
we will just add special cases as-needed.

@subsection{Unhandled}

We currently don't handle structure types, prefab structures, hash tables,
syntax objects and lots of other types.

On the other hand, we can't handle fixed-length @tc[(Vector ...)] types, because
occurrence typing currently can't track which case we are in when we check the
length with @tc[(vector-length constant)]. We also can't handle functions, for
hopefully obvious reasons.

@; TODO: insert a link to the type-expander document in the paragraph below.

We run into a problem though with types declared via define-type without
informing the type-expander. The type-expander handles these by expanding just
their arguments, and leaving the type untouched, but we can't ignore them in our
case.

For all these other cases, we'll just check that they don't contain any
reference to a link-request type.

@chunk[<fold-type-cases2>
       [other
        (fold-check-no-link-requests #'other)
        #'other]]

The checker below is approximate, and is just meant to catch the error as soon
as possible, and we include a fall-back case for anything we couldn't handle
properly. If we let a link-request slip, it should be caught by the type
checker, unless it is absorbed by a larger type, like in
@tc[(U Any link-request)], in which case it doesn't matter.

@chunk[<fold-check-no-link-requests>
       (define (fold-check-no-link-requests stx)
         (syntax-parse stx
           [(~and whole (~or (~literal node/link-request) ...))
            (raise-syntax-error
             'graph
             "Found a link request buried somewhere I can't access"
             whole)]
           [(~and whole (t ...))
            (stx-map fold-check-no-link-requests #'(t ...))]
           [whole whole]))]

@section[#:tag "graph|TODO2"]{TODO}

@chunk[<multiassoc-syntax>
       (define (multiassoc-syntax query alist)
         (map stx-cdr
              (filter (λ (xy) (free-identifier=? query (stx-car xy)))
                      (syntax->list alist))))
       (define (assoc-syntax query alist)
         (let ([res (assoc query (map syntax-e (syntax->list alist))
                           free-identifier=?)])
           (unless res (raise-syntax-error '? (format "Can't find ~a in ~a"
                                                      query
                                                      alist)))
           (cdr res)))]

@CHUNK[<old-make-graph-database>
       ;; The actual traversal code:
       ;; TODO: write a tail-recursive version, it's cleaner than using set!.
       (: make-graph-database
          (→ root-transform.param.type ...
             (case→ (→ 'node.name (Listof (Pairof Any node.incomplete)))
                    ...)))
       (define (make-graph-database root-transform.param.name ...)
         (let ([pending : (Listof (U node.link-request ...))
                (list (cons (list 'root-transform.name
                                  root-transform.param.name ...)
                            (λ () (root-transform.function
                                   root-transform.param.name ...))))]
               [all-transformed : (Listof (Pairof Symbol Any)) '()]
               ;; the key is actually the second element in a
               ;; link-request-???, but should be just a number like in
               ;; the C# version.
               [node.transformed : (Listof (Pairof Any node.incomplete)) '()]
               ...)
           (do : (case→ (→ 'node.name (Listof (Pairof Any node.incomplete)))
                        ...)
             ()
             [(null? pending)
              (ann (λ (selector)
                     (cond [(eq? selector 'node.name) node.transformed] ...))
                   (case→ (→ 'node.name (Listof (Pairof Any node.incomplete)))
                          ...))]
             (let ((request (car pending)))
               ;; Must be immediately after the (let (...), because we cons to
               ;; that list in the block below.
               (set! pending (cdr pending))
               ;; Skip already-transformed link requests. TODO: map a number
               ;; for each.
               (unless (member (car request) all-transformed)
                 ;; Call the lambda-part of the request.
                 (let ([transformed ((cdr request))])
                   (cond
                     [(eq? (car transformed) 'node.name)
                      (set! pending
                            (list* ((cdr transformed)
                                    'node/field-filter-out-primitives/name)
                                   ...
                                   pending))
                      (set! all-transformed (cons (car request)
                                                  all-transformed))
                      (set! node.transformed
                            (cons (cons (car request) (cdr transformed))
                                  node.transformed))]
                     ...
                     ;; Make sure all cases are treated, at compile-time.
                     [else (typecheck-fail #'#,stx
                                           "incomplete coverage")])))))))]

@section{Tests}

@chunk[<test-graph>
       (values)]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket/sequence
                                ;; in-syntax on older versions
                                ;;;unstable/sequence
                                syntax/parse
                                syntax/parse/experimental/template
                                racket/syntax
                                racket/function
                                racket/pretty
                                (submod "../lib/low.rkt" untyped)
                                "../lib/untyped.rkt")
                    (prefix-in DEBUG-tr: typed/racket)
                    syntax/parse
                    "../lib/low.rkt"
                    "adt.lp2.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           (provide make-graph-constructor
                    #|graph|#)
           
           (begin-for-syntax
             <multiassoc-syntax>)
           <pre-declare-transform/link-request>
           
           <make-graph-constructor>
           
           #|<graph>|#)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    "../type-expander/type-expander.lp2.rkt"
                    "../lib/test-framework.rkt")
           
           ;; Debug
           <pre-declare-transform/link-request>
           (require syntax/parse
                    "../lib/low.rkt"
                    "adt.lp2.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           ;;
           
           <test-graph>
           <test-make-graph-constructor>))]
