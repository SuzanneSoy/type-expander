#lang scribble/lp2
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

Here is the root mapping for our example. It calls the @tc[street] and
@tc[person] mappings.

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
@; And we are already merging all equal? link-requests, so there shouldn't be
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

The first case shows that ce can use @tc[street] as any other function, passing
it to @tc[curry], and calling remove-duplicates on the results. Note that reach
placeholder returned by @tc[street] will contain all information passed to it,
here a street name and @tc[c]. Two placeholders for @tc[street] will therefore
be @tc[equal?] if and only if all the arguments passed to @tc[street] are
@tc[equal?]. The placeholders also include a symbol specifying which mapping was
called, so two placeholders for two mappings called with identical parameters
will not be @tc[equal?].

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

@subsection{The macro's syntax}

We use a simple syntax for @tc[make-graph-constructor], and make it more
flexible through wrapper macros.

@chunk[<signature>
       (make-graph-constructor
        [node type ...]
        root:expr
        [(mapping [param (~literal :) param-type] ...) (~literal :) result-type
         body]
        ...)]

@chunk[<make-graph-constructor>
       (define-syntax/parse <signature>
         #'(void))]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              racket/syntax
                              "../lib/low-untyped.rkt")
                  "../lib/low.rkt")
         
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