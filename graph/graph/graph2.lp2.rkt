#lang debug scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Graph library}

@(table-of-contents)

@; TODO: allow a mapping to return a new placeholder, in order to act as a
@; redirect. All references to the old placeholder will act as if they were to
@; the new placeholder.

@section{Introduction}

This module provides a @tc[graph] macro which helps constructing immutable
graphs (using lambdas to defer potentially cyclic references).

@subsection{The graph's type}

Each node type in the graph is a variant's constructor, tagged with the node
name. For example, a graph representing a city and its inhabitants could use
these variants:

@chunk[<example-variants>
       [City #|[streets : (Listof Street)]|# [inhabitants : (Listof Person)]]
       #|DEBUG|#
       #|[Street [houses : (Listof House)]]
       [House [owner : Person] [location : Street]]|#
       [Person [name : String]]]

Notice the cycle in the type: a street contains houses, which are located on the
same street.

@subsection{Example usage}

We will start with a running example, which will help us both show the macro's
syntax, and show some of the key advantages offered by this graph library.

@subsubsection{A seed from which to unravel the graph: the root parameters}

In order to build a graph with that type, we start from the root parameters.
Here, we will take a representation of the city as a list of
@tc[(street . person-name)] pairs, and will convert it to a more convenient
graph representation. Our single root parameter will thus be the whole list:

@chunk[<example-root>
       '(["Amy" . "Ada street"]
         ["Jack" . "J street"]
         ["Anabella" . "Ada street"])]

We then provide a mapping from the root parameter to the root node, in our case
@tc[City]. When processing the root parameter, one can call mappings that will
create other nodes.

@subsubsection{Mapping the root parameters to the root node}

Here is the root mapping for our example. It maps over the list of names and
street names @tc[c], and calls for each element the @tc[street] and @tc[person]
mappings.

@; Would be nicer with (map (∘ (curry street c) my-car) c)), but that doesn't
@; typecheck (yet).
@chunk[<example-mappings>
       [(city [c : (Listof (Pairof String String))]) : City
        (City #|DEBUG|##|(remove-duplicates (map (curry street c) (cars c)))|#
         (remove-duplicates (map Person (cdrs c))))]]

@subsubsection{More mappings}

Next, we write the @tc[street] mapping, which takes a street name and the whole
city @tc[c] in list form, and creates a @tc[Street] node.

@chunk[<example-mappings>
       #|DEBUG|#
       #|[(street [c : (Listof (Pairof String String))] [s : String]) : Street
       (Street (map (curry (curry house s) c)
       (cars (filter (λ ([x : (Pairof String String)])
       (equal? (cdr x) s))
       c))))]|#]

The @tc[house] mapping calls back the @tc[street] mapping, to store for each
house a reference to the containing street. Normally, this would cause infinite
recursion in an eager language, like @tc[typed/racket]. However, the mappings
aren't called directly, and instead the @tc[street] function here returns a
placeholder. This allows us to not worry about mutually recursive mappings: a
mapping can be called any number of times with the same data, it will actually
only be run once.

The @tc[make-graph-constructor] macro will post-process the result of each
mapping, and replace the placeholders with promises for the the result of the
mapping. The promises are not available during graph construction, so there is
no risk of forcing one before it is available.

Finally, we write the @tc[house] mapping.

@chunk[<example-mappings>
       #|
       [(house [s : String] [c : (Listof (Pairof String String))] [p : String])
       : House
       (House (Person p) (street c s))]|#
       #|DEBUG|#]

Notice how we are calling directly the @tc[Person] constructor above. We also
called it directly in the @tc[city] mapping. Since @tc[Person] does not contain
references to @tc[House], @tc[Street] or @tc[City], we do not need to delay
creation of these nodes by calling yet another mapping.

@; TODO: above: Should we merge two identical instances of Person? They won't
@; necessarily be eq? if they contain cycles deeper in their structure, anyway.
@; And we are already merging all equal? placeholders, so there shouldn't be
@; any blowup in the number of nodes.
@; It would probably be better for graph-map etc. to have all the nodes in the
@; database, though.

The number and names of mappings do not necessarily reflect the graph's type.
Here, we have no mapping named @tc[person], because that node is always created
directly. Conversely, we could have two mappings, @tc[big-street] and
@tc[small-street], with different behaviours, instead of passing an extra
boolean argument to @tc[street].

@subsubsection{Making a constructor for the graph}

@chunk[<make-constructor-example>
       (make-graph-constructor (<example-variants>)
                               <example-root>
                               <example-mappings>)]

@subsubsection{Creating a graph instance}

@chunk[<use-example>
       (define g <make-constructor-example>)]

@subsection{More details on the semantics}

Let's take a second look at the root mapping:

@chunk[<example-mappings-2>
       [(city [c : (Listof (Pairof String String))])
        (City (remove-duplicates (map (∘ (curry street c) car) c))
              (remove-duplicates (map (∘ Person cdr) c)))]]

The first case shows that we can use @tc[street] as any other function, passing
it to @tc[curry], and calling @tc[remove-duplicates] on the results. Note that
each placeholder returned by @tc[street] will contain all information passed to
it, here a street name and @tc[c]. Two placeholders for @tc[street] will
therefore be @tc[equal?] if and only if all the arguments passed to @tc[street]
are @tc[equal?]. The placeholders also include a symbol specifying which mapping
was called, so two placeholders for two different mappings will not be
@tc[equal?], even if identical parameters were supplied.

The second case shows that we can also directly call the constructor for the
@tc[Person] node type. If that type contains references to other nodes, the
constructor here will actually accept either a placeholder, or an actual
instance, which itself may contain placeholders.

The node type allowing placeholders is derived from the ideal type given above.
Here, the type for @tc[Person] is @tc[[String]], so there are no substitutions
to make. On the contrary, the type for @tc[City], originally expressed as
@tc[[(Listof Street) (Listof Person)]], will be rewritten into
@tc[[(Listof (U Street Street-Placeholder))
     (Listof (U Person Person-Placeholder))]].

The @tc[rewrite-type] module we use to derive types with placeholders from the
original ones only handles a handful of the types offered by @tc[typed/racket].
In particular, it does not handle recursive types described with @tc[Rec] yet.

@section{Implementation}

In this section, we will describe how the @tc[make-graph-constructor] macro is
implemented.

@subsection{The different types of a node and mapping}

A single node name can refer to several types:

@itemlist[
 @item{The @emph{ideal} type, expressed by the user, for example
  @racket[[City (Listof Street) (Listof Person)]], it is never used as-is in
  practice}
 @item{The @emph{incomplete} type, in which references to other node types are
  allowed to be either actual (@racket[incomplete]) instances, or placeholders.
  For example, @racket[[City (Listof (U Street Street/placeholder-type))
                        (Listof (U Person Person/placeholder-type))]].}
 @item{The @emph{with-promises} type, in which references to other node types
  must be replaced by a @racket[Promise] for the target node's
  @racket[with-promises] type. For example,
  @racket[[City (Listof (Promise Street/with-promises-type))
           (Listof (Promise Person/with-promises-type))]].}]

When the user code calls a mapping, a placeholder is instead returned. We
therefore will have one placeholder type per mapping. Mappings come in various
flavours too:

@itemlist[
 @item{The \emph{placeholder} type and constructor, which just store the
  arguments for the mapping along with its name}
 @item{The mapping function's \emph{body}, which takes some parameters and
  returns a node (this is the code directly provided by the user)}]

@subsection{The macro's syntax}

We use a simple syntax for @tc[make-graph-constructor], and make it more
flexible through wrapper macros.

@chunk[<signature>
       (make-graph-constructor
        ([node [field-name:id (~literal :) field-type:expr] ...] ...)
        (root-expr:expr ...)
        [(mapping:id [param:id (~literal :) param-type:expr] ...)
         (~literal :) result-type:expr
         . mapping-body]
        ...)]

The macro relies heavily on two sidekick modules: @tc[rewrite-type], and
@tc[fold-queue]. The former will allow us to derive from the ideal type of a
node the incomplete type and the with-promises type. It will also allow us to
search in instances of incomplete nodes, in order to extract the placehoders,
and replace these parts with promises. The latter, @tc[fold-queue], will be used
to process all the pending placeholders, with the possibility to enqueue new
ones as these placeholders are discovered inside incomplete nodes.

When the graph constructor is called with the arguments for the root parameters,
it is equivalent to make and then resolve an initial placeholder. We will use a
function from the @tc[fold-queue] library to process the queues of pending
placeholders, starting with a queue containing only that root placeholder.
We will have one queue for each placeholder type.@note{It we had only one queue,
 we would have only one collection of results, and would need a @racket[cast]
 when extracting nodes from the collection of results.} The
queues' element types will therefore be these placeholder types.

@chunk[<fold-queue-type-element>
       mapping/placeholder-type]

The return type for each queue will be the corresponding with-promises type. The
fold-queues function will therefore return a vector of with-promises nodes.

@chunk[<fold-queue-type-result>
       <with-promises-type>]

@; Problem: how do we ensure we return the right type for the root?
@; How do we avoid casts when doing look-ups?
@; We need several queues, handled in parallel, with distinct element types.
@; * Several result aggregators, one for each type, so we don't have to cast
@; * Several queues, so that we can make sure the root node is of the expected
@;   type.

@; TODO: clarity.
@; The @tc[fold-queues] function allows us to associate each element with a tag,
@; so that, inside the processing function and outside, we can refer to an
@; element using this tag, which can be more lightweight than keeping a copy of
@; the element.
@; 
@; We will tag our elements with an @tc[Index], which prevents memory leakage:
@; if we kept references to the original data added to the queue, a graph's
@; representation would hold references to its input, which is not the case when
@; using simple integers to refer to other nodes, instead of using the input for
@; these nodes. Also, it makes lookups in the database much faster, as we will
@; be able to use an array instead of a hash table.

@subsection{The queues of placeholders}

The fold-queus macro takes a root element, in our case the root placeholder,
which it will insert into the first queue. The next clauses are the queue
handlers, which look like function definitions of the form
@tc[(queue-name [element : element-type] Δ-queues enqueue)]. The @tc[enqueue]
argument is a function used to enqueue elements and get a tag in return, which
can later be used to retrieve the processed element.

Since the @tc[enqueue] function is pure, it takes a parameter of the same type
as @tc[Δ-queues] representing the already-enqueued elements, and returns a
modified copy, in addition to the tag. The queue's processing body should return
the latest @tc[Δ-queues] in order to have these elements added to the queue.

@chunk[<fold-queue>
       (fold-queues <root-placeholder>
                    [(mapping [e : <fold-queue-type-element>] get-tag Δ-queues)
                     : <fold-queue-type-result>
                     <fold-queue-body>]
                    ...)]

@subsection{Making placeholders for mappings}

We start creating the root placeholder which we provide to @tc[fold-queues].

@chunk[<root-placeholder>
       (root/make-placeholder root-expr ...)]

To make the placeholder, we will need a @tc[make-placeholder] function for each
@tc[mapping]. We define the type of each placeholder (a list of arguments,
tagged with the @tc[mapping]'s name), and a constructor:

@; TODO: just use (variant [mapping param-type ...] ...)

@chunk[<define-mapping-placeholder>
       (define-type mapping/placeholder-type (List 'placeholder
                                                   'mapping
                                                   param-type ...))
       
       (: mapping/make-placeholder (→ param-type ... mapping/placeholder-type))
       (define (mapping/make-placeholder [param : param-type] ...)
         (list 'placeholder 'mapping param ...))]

The code above needs some identifiers derived from @tc[mapping] names:

@chunk[<define-ids>
       (define-temp-ids "~a/make-placeholder" (mapping ...))
       (define-temp-ids "~a/placeholder-type" (mapping ...))
       (define/with-syntax (root/make-placeholder . _)
         #'(mapping/make-placeholder ...))]

@subsection{Making with-promises nodes}

We derive the @tc[with-promises] type from each @emph{ideal} node type using
the @tc[tmpl-replace-in-type] template metafunction from the rewrite-type
library. We replace all occurrences of a @tc[node] name with a @tc[Promise] for
that node's @tc[with-promises] type.

@; TODO: use a type-expander here, instead of a template metafunction.

@CHUNK[<define-with-promises-nodes>
       (define-type field/with-promises-type
         (tmpl-replace-in-type field-type
                               [node (Promise node/with-promises-type)]
                               …))
       …
       
       (define-type node/with-promises-type (List 'with-promises
                                                  'node
                                                  field/with-promises-type …))
       
       (: node/make-with-promises (→ field/with-promises-type …
                                     node/with-promises-type))
       (define (node/make-with-promises field-name …)
         (list 'with-promises 'node field-name …))]

The code above needs some identifiers derived from @tc[node] and
@tc[field-name]s:

@chunk[<define-ids>
       (define-temp-ids "~a/make-with-promises" (node ...))
       (define-temp-ids "~a/with-promises-type" (node ...))
       (define/with-syntax ((field/with-promises-type …) …)
         (stx-map generate-temporaries #'((field-name …) …)))]

@subsection{Making incomplete nodes}

We derive the @tc[incomplete] type from each @emph{ideal} node type using
the @tc[tmpl-replace-in-type] template metafunction from the rewrite-type
library. We replace all occurrences of a @tc[node] name with a union of the
node's @tc[incomplete] type, and all compatible @tc[placeholder] types.

TODO: for now we allow all possible mappings, but we should only allow those
which return type is the desired node type.

@; TODO: use a type-expander here, instead of a template metafunction.

@CHUNK[<define-incomplete-nodes>
       (define-type field/incomplete-type <field/incomplete-type>)
       …
       
       (define-type node/incomplete-type
         (Pairof 'incomplete (Pairof 'node (List field/incomplete-type …))))
       
       (: node/make-incomplete (→ field/incomplete-type … node/incomplete-type))
       (define (node/make-incomplete field-name …)
         (list 'incomplete 'node field-name …))]

Since the incomplete type for fields will appear in two different places, above
and in the incomplete-to-with-promises conversion routine below, we write it in
a separate chunk:

@chunk[<field/incomplete-type>
       (tmpl-replace-in-type field-type
                             [node (U node/incomplete-type
                                      node/compatible-placeholder-types …)]
                             …)]

We must however compute for each node the set of compatible placeholder types.
We do that

@chunk[<define-compatible-placeholder-types>
       (define/with-syntax ((node/compatible-placeholder-types ...) ...)
         (for/list ([x (in-syntax #'(node ...))])
           (multiassoc-syntax
            x
            #'([result-type . (List 'placeholder 'mapping param-type ...)]
               …))))]

The multiassoc-syntax function used above filters the associative syntax list
and returns the @tc[stx-cdr] of the matching elements, therefore returning a
list of @tc[mapping/placeholder-type]s for which the @tc[result-type] is the
given @tc[node] name.

@chunk[<multiassoc-syntax>
       (define (multiassoc-syntax query alist)
         (map stx-cdr
              (filter (λ (xy) (free-identifier=? query (stx-car xy)))
                      (syntax->list alist))))
       
       (define (cdr-assoc-syntax query alist)
         (stx-cdr (findf (λ (xy) (free-identifier=? query (stx-car xy)))
                         (syntax->list alist))))
       
       (define-template-metafunction (tmpl-cdr-assoc-syntax stx)
         (syntax-parse stx
           [(_ query [k . v] …)
            (cdr-assoc-syntax #'query #'([k . v] …))]))]

The code above also needs some identifiers derived from @tc[node] and
@tc[field-name]s:

@; TODO: format-ids doesn't accept arbitrary values. Should we change it?
@chunk[<define-ids>
       (define-temp-ids "~a/make-incomplete" (node …))
       (define-temp-ids "~a/incomplete-type" (node …))
       (define-temp-ids "~a/incomplete-fields" (node …))
       (define/with-syntax ((field/incomplete-type …) …)
         (stx-map generate-temporaries #'((field-name …) …)))]

@subsection{Converting incomplete nodes to with-promises ones}

@chunk[<convert-incomplete-to-with-promises>
       [node/incomplete-type
        node/with-promises-type
        (λ (x) (and (pair? x)
                    (eq? (car x) 'incomplete)
                    (pair? (cdr x))
                    (eq? (cadr x) 'node)))
        (λ ([x : node/incomplete-type] [acc : Void])
          (if (eq? (car x) 'incomplete)
              <convert-incomplete-successor>
              <convert-placeholder-successor>))]]

@chunk[<convert-incomplete-successor>
       (error (~a "Not implemented yet " x))]

@chunk[<convert-placeholder-successor>
       (% tag new-Δ-queues = (get-tag (cadr x) x Δ-queues)
          (error (~a "Not implemented yet " x)))]


@subsection{Processing the placeholders}

@; TODO: also allow returning a placeholder (which means we should then
@; process that placeholder in turn). The placeholder should return the
@; same node type, but can use a different mapping?
@; Or maybe we can do this from the ouside, using a wrapper macro?

@CHUNK[<fold-queue-body>
       (let ([mapping-result (apply mapping/function (cddr e))])
         (tmpl-fold-instance (tmpl-cdr-assoc-syntax
                              result-type
                              [node . (List 'incomplete
                                            'node
                                            <field/incomplete-type> …)]
                              …)
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             Void
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             <convert-incomplete-to-with-promises> …))
       'todo!]

@section{The mapping functions}

We define the mapping functions as they are described by the user, with an
important change: Instead of returning an @emph{ideal} node type, we expect them
to return an incomplete node type.

@chunk[<define-mapping-function>
       (define-type mapping/incomplete-result-type
         (tmpl-replace-in-type result-type
                               [node node/incomplete-type]
                               …))
       
       (: mapping/function (→ param-type … mapping/incomplete-result-type))
       (define mapping/function
         (let ([mapping mapping/make-placeholder]
               …
               [node node/make-incomplete]
               …)
           (λ (param …)
             . mapping-body)))]

@chunk[<define-ids>
       (define-temp-ids "~a/function" (mapping ...))
       (define-temp-ids "~a/incomplete-result-type" (mapping ...))]

@section{Temporary fillers}

@chunk[<with-promises-type>
       Any]


@section{Putting it all together}

@chunk[<make-graph-constructor>
       (define-syntax/parse <signature>
         <define-ids>
         <define-compatible-placeholder-types>
         ((λ (x) (pretty-write (syntax->datum x)) x)
          (template
           (let ()
             (begin <define-mapping-placeholder>) …
             (begin <define-with-promises-nodes>) …
             (begin <define-incomplete-nodes>) …
             (begin <define-mapping-function>) …
             <fold-queue>))))]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              racket/syntax
                              syntax/stx
                              syntax/parse/experimental/template
                              racket/sequence
                              racket/pretty; DEBUG
                              alexis/util/threading; DEBUG
                              "rewrite-type.lp2.rkt"
                              "../lib/low-untyped.rkt")
                  alexis/util/threading; DEBUG
                  "fold-queues.lp2.rkt"
                  "rewrite-type.lp2.rkt"
                  "../lib/low.rkt")
         
         (begin-for-syntax
           <multiassoc-syntax>)
         
         (provide make-graph-constructor)
         <make-graph-constructor>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  "fold-queues.lp2.rkt"; DEBUG
                  "rewrite-type.lp2.rkt"; DEBUG
                  "../lib/low.rkt"; DEBUG
                  typed/rackunit)
         
         <use-example>
         
         g



























         

         (require (submod ".." doc)))]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]