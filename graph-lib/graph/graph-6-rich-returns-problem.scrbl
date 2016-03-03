#lang scribble/lp2

@section{Problem 1}

For the rich-returns graph, we want to detect nodes from the
first step, and convert them to the appropriate value for
the second step.

The issue is that @racket[tmpl-replace-in-instance]
requires us to provide a predicate which matches the types
we should replace.

Variants should all be subtypes of the global Tagged type,
so that we can access their contents via the @racket[dot]
operator without having to know the variant's tag (when
accessing a node's content: nodes are tagged).

@section{Problem 2}

Graph subtyping: It seems reasonable to not allow arbitrary
subtyping for graphs, as a pass might not be semantically
correct if it ignores some of the nodes of the graph. For
example, let's say we have a replace-arithmetic pass:

@chunk[<replace-arithmetic-pass>
       (define-pass replace-arithmetic
         [(add a b) (arithmetic 'add a b)]
         [(mul a b) (arithmetic 'mul a b)]
         [(div a b) (arithmetic 'div a b)])]

If the pass above is called on a graph @racket[g-div] where
divisions are already marked as safe (@racket[b] is never 
@racket[0]) or unsafe (@racket[b] can be @racket[0]), the
resulting graph will have @racket[arithmetic] nodes
@emph{and} @racket[div-unsafe] ones, whereas one would
semantically expect the @racket[div-unsafe] nodes to have
been merged too:

@chunk[<g-div>
       (define-graph (g-div)
         (var [name : String])
         (add [a : var] [b : var])
         (mul [a : var] [b : var])
         (div [a : var] [b : var])
         (div-unsafe [a : var] [b : var] [on-error-message : String])
         other-nodes …)]

We do however wish to be able to test a pass without having
to care about the irrelevant nodes. We could specify a union
of graph types when writing the pass. The 
@racket[replace-arithmetic] pass would then be declared as
follows:

@chunk[<replace-arithmetic-pass>
       (define-pass (replace-arithmetic [x : (U g g-test)]) : g-arith
         [(add a b) (arithmetic 'add a b)]
         [(mul a b) (arithmetic 'mul a b)]
         [(div a b) (arithmetic 'div a b)])]

Where @racket[g] has been declared using:

@chunk[<g>
       (define-graph (g)
         (var [name : String])
         (add [a : var] [b : var])
         (mul [a : var] [b : var])
         (div [a : var] [b : var])
         other-nodes …)]

And @racket[g-test], which does not contain the 
@racket[other-nodes] has been declared using:

@chunk[<g-test>
       (define-graph (g-test)
         (var [name : String])
         (add [a : var] [b : var])
         (mul [a : var] [b : var])
         (div [a : var] [b : var]))]

The @racket[g-test] declaration could easily be derived
from the pass declaration, by removing the node types not
mentionned within.

@subsection{Graph operations}

The graph operations should not require specifying the type
however: ideally, one should write the input graph type name
only in the parameter list of the pass, and not have to
refer to it within the pass body for common operations. Thes
operations include:

@chunk[<*>
       (begin)]