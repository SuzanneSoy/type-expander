#lang debug scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Graph library}

@(table-of-contents)

@section{Introduction}

This module provides a @tc[graph] macro which helps constructing immutable
graphs (using lambdas to defer potentially cyclic references).

@subsection{The graph's type}

Each node type in the graph is a variant's constructor, tagged with the node
name. For example, a graph representing a city and its inhabitants could use
these variants:

@chunk[<example-variants>
       [City (Listof Street) (Listof Person)]
       [Street (Listof House)]
       [House Person Street]
       [Person String]]

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

@chunk[<example-mappings>
       [(city [c : (Listof (Pairof String String))]) : City
        (City (remove-duplicates (map (∘ (curry street c) car) c))
              (remove-duplicates (map (∘ Person cdr) c)))]]

@subsubsection{More mappings}

Next, we write the @tc[street] mapping, which takes a street name and the whole
city @tc[c] in list form, and creates a @tc[Street] node.

@chunk[<example-mappings>
       [(street [c : (Listof (Pairof String String))] [s : String]) : Street
        (Street (map (∘ (curry house s c) car)
                     (filter (λ ([x : (Pairof String String)])
                               (equal? (cdr x) s))
                             c)))]]

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
       [(house [s : String] [c : (Listof (Pairof String String))] [p : String])
        : House
        (House (Person p) (street c s))]]

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
  @racket[[City (Listof Street) (Listof Person)]]}
 @item{The @emph{incomplete} type, in which references to other node types are
  allowed to be either actual instances, or placeholders. For example,
  @racket[[City (Listof (U Street Street-Placeholder))
           (Listof (U Person Person-Placeholder))]].}
 @item{The @emph{with-promises} type, in which references to other node types
  must be replaced by promises for these. For example,
  @racket[[City (Listof (Promise Street)) (Listof (Promise Person))]].}]

When the user code calls a mapping, a placeholder is instead returned. We
therefore will have one placeholder type per mapping.

@subsection{The macro's syntax}

We use a simple syntax for @tc[make-graph-constructor], and make it more
flexible through wrapper macros.

@chunk[<signature>
       (make-graph-constructor
        [node type ...]
        (root-expr:expr ...)
        [(mapping [param (~literal :) param-type] ...) (~literal :) result-type
         body]
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
       <placeholder-type>]

The return type for each queue will be the corresponding with-promises type.

@chunk[<fold-queue-type-result>
       <with-promises-type>]

@; Problem: how do we ensure we return the right type for the root?
@; How do we avoid casts when doing look-ups?
@; We need several queues, handled in parallel, with distinct element types.
@; * Several result aggregators, one for each type, so we don't have to cast
@; * Several queues, so that we can make sure the root node is of the expected
@;   type.

@; TODO: clarity.
@; The @tc[fold-queues] function allows us to associate each element with a tag, so
@; that, inside the processing function and outside, we can refer to an element
@; using this tag, which can be more lightweight than keeping a copy of the
@; element.
@; 
@; We will tag our elements with an @tc[Index], which prevents memory leakage: if
@; we kept references to the original data added to the queue, a graph's
@; representation would hold references to its input, which is not the case when
@; using simple integers to refer to other nodes, instead of using the input for
@; these nodes. Also, it makes lookups in the database much faster, as we will be
@; able to use an array instead of a hash table.

@subsection{The queues of placeholders}

@chunk[<root-placeholder>
       (make-placeholder root-expr ...)]

@chunk[<fold-queue>
       (fold-queues <root-placeholder>
                    [(mapping [e : <fold-queue-type-element>] get-tag Δ-queues)
                     : <fold-queue-type-result>
                     'todo!]
                    ...)]


@section{Making placeholders}

@; TODO: make a template library that implicitly creates temporaries for
@; foo/bar, when foo is a syntax parameter.

@chunk[<define-ids>
       (define-temp-ids mapping "~a/make-placeholder")]

@chunk[<define-ids>
       (define-temp-ids mapping "~a/placeholder-type")]

@chunk[<define-placeholders>
       (define-type mapping/placeholder-type (List 'mapping param-type ...))
       
       (: mapping/make-placeholder (→ param-type ... mapping/placeholder-type))
       (define (mapping/make-placeholder [param : param-type] ...)
         (list 'mapping param ...))]

@section{Temporary fillers}

@chunk[<placeholder-type>
       Any]

@chunk[<with-promises-type>
       Any]


@section{Bits and pieces}

@chunk[<make-graph-constructor>
       (define-syntax/parse <signature>
         <define-ids>
         #'(begin
             (begin <define-placeholders>) ...
             <fold-queue>))]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              racket/syntax
                              "../lib/low-untyped.rkt")
                  "fold-queues.lp2.rkt"
                  "rewrite-type.lp2.rkt"
                  "../lib/low.rkt")
         
         (provide fold-queues)
         
         (provide make-graph-constructor)
         <make-graph-constructor>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  typed/rackunit)
         
         <make-constructor-example>
         
         (require (submod ".." doc)))]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]