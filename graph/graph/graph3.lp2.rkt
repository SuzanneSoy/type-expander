#lang debug scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@(define (comment . _) "")

@title[#:style manual-doc-style]{Graph library}

@(table-of-contents)

@; TODO: allow a mapping to return a new placeholder, in order to act as a
@; redirect. All references to the old placeholder will act as if they were to
@; the new placeholder.

@section{Introduction}

This module provides a @tc[graph] macro which helps constructing immutable
graphs (using lambdas to defer potentially cyclic references).

@subsection{Example usage}

We will start with a running example, which will help us both show the macro's
syntax, and see some of the key advantages offered by this graph library.

@subsection{The graph's type}

Each node type in the graph is a variant's constructor, tagged with the node
name. For example, a graph representing a city and its inhabitants could use
these constructors:

@chunk[<example-variants>
       [City [streets : (Listof Street)] [people : (Listof Person)] <m-city>]
       [Street [houses : (Listof House)] <m-street>]
       [House [owner : Person] [location : Street] <m-house>]
       [Person [name : String] <m-person>]]

Notice the cycle in the type: a street contains houses, which are located on the
same street.

@subsubsection{A seed from which to unravel the graph: the root parameters}

In order to build a graph with that type, we start from the root parameters.
Here, we will take a representation of the city as a list of
@tc[(street . person-name)] pairs, and will convert it to a more convenient
graph representation. Our single root parameter will thus be the whole list:

@chunk[<example-root>
       '(["Amy" . "Ada street"]
         ["Jack" . "J street"]
         ["Anabella" . "Ada street"])]

We will then provide a mapping from the root parameter to the root node, in our
case @tc[City]. When processing the root parameter, one can call other mappings
that will create their corresponding nodes.

@subsubsection{Mapping the root parameters to the root node}

Here is the root mapping for our example. It maps over the list of names and
street names @tc[c], and calls for each element the @tc[m-street] and
@tc[m-person] mappings.

@; Would be nicer with (map (∘ (curry street c) car) c)), but that doesn't
@; typecheck (yet).
@chunk[<m-city>
       [(m-city [c : (Listof (Pairof String String))])
        (City (remove-duplicates (map (curry m-street c) (cars c)))
              (remove-duplicates (map m-person (cdrs c))))]]

@subsubsection{More mappings}

Next, we write the @tc[m-street] mapping, which takes a street name and the
whole city @tc[c] in list form, and creates a @tc[Street] node.

@chunk[<m-street>
       [(m-street [c : (Listof (Pairof String String))] [s : String])
        (Street (map (curry (curry m-house s) c)
                     (cars (filter (λ ([x : (Pairof String String)])
                                     (equal? (cdr x) s))
                                   c))))]]

The @tc[m-house] mapping defined below calls back the @tc[m-street] mapping, to
store for each house a reference to the containing street. Normally, this would
cause infinite recursion in an eager language, like @tc[typed/racket]. However,
the mappings aren't called directly, and instead, in the body of @tc[m-house],
@tc[m-street] is shadowed by a function which returns a placeholder. This allows
us to not worry about mutually recursive mappings: a mapping can be called any
number of times with the same data, it will actually only be run once.

The @tc[make-graph-constructor] macro will post-process the result of each
mapping, and replace the placeholders with promises for the the result of the
mapping. The promises are not available during graph construction, so there is
no risk of forcing one before it is available.

We can now write the @tc[m-house] and @tc[m-person] mappings.

@chunk[<m-house>
       [(m-house [s : String]
                 [c : (Listof (Pairof String String))]
                 [p : String])
        (House (m-person p) (m-street c s))]]

@chunk[<m-person>
       [(m-person [p : String])
        (Person p)]]

@subsubsection{Creating an instance of the graph}

For now, we will supply directly the root arguments to the @tc[make-graph]
macro, as well as the node types and mappings. We can later curry the macro, so
that it first takes the node types and mappings, and produces a lambda taking
the root arguments as parameters.

@chunk[<use-example>
       (define g
         (make-graph (<example-variants>) <example-root>))]

@subsection{More details on the semantics}

Let's take a second look at the root mapping:

@chunk[<m-city-2>
       [(m-city [c : (Listof (Pairof String String))])
        (City (remove-duplicates (map (curry m-street c) (cars c)))
              (remove-duplicates (map m-person (cdrs c))))]]

As this example shows, we can use @tc[m-street] as any other function, passing
it to @tc[curry], and calling @tc[remove-duplicates] on the results. Note that
each placeholder returned by @tc[m-street] will contain all information passed
to it, here a street name and @tc[c]. Two placeholders for @tc[m-street] will
therefore be @tc[equal?] if and only if all the arguments passed to
@tc[m-street] are @tc[equal?]. The placeholders also include a symbol specifying
which mapping was called, so two placeholders for two different mappings will
not be @tc[equal?], even if identical parameters were supplied.

The node type allowing placeholders is derived from the ideal type given above.
Here, the type for @tc[Person] is @tc[[Person [name : String]]], so there are no
substitutions to make. Conversely, the type for @tc[City], originally expressed
as @tc[[(Listof Street) (Listof Person)]], will be rewritten as
@tc[[(Listof Street/placeholder-type) (Listof Person/placeholder-type)]].

The @tc[rewrite-type] module, which we use to derive types with placeholders
from the ideal ones, only handles a handful of the types offered by
@tc[typed/racket]. In particular, it does not handle recursive types described
with @tc[Rec] yet.

@section{Implementation}

In this section, we will describe how the @tc[make-graph] macro is implemented.

@subsection{The macro's syntax}

We use a simple syntax for @tc[make-graph], and make it more flexible through
wrapper macros.

@chunk[<signature>
       (make-graph ([node <field-signature> … <mapping-declaration>]
                    …)
                   (root-expr:expr …))]

Where @tc[<field-signature>] is:

@chunk[<field-signature>
       [field:id (~literal :) field-type:expr]]

And @tc[<mapping-declaration>] is:

@chunk[<mapping-declaration>
       ((mapping:id [param:id (~literal :) param-type:expr] …)
        . mapping-body)]

@subsection{The different types of a node}

A single node name can refer to several types:

@itemlist[
 @item{The @emph{ideal} type, expressed by the user, for example
  @racket[[City (Listof Street) (Listof Person)]], it is never used as-is in
  practice}
 @item{The @emph{placeholder} type, type and constructor, which just store the
  arguments for the mapping along with a tag indicating the node name}
 @item{The @emph{incomplete} type, in which references to other node types are
  allowed to be either actual (@racket[incomplete]) instances, or placeholders.
  For example, @racket[[City (Listof (U Street Street/placeholder-type))
                        (Listof (U Person Person/placeholder-type))]].}
 @item{The @emph{with-indices} type, in which references to other node types
  must be replaced by an index into the results list for the target node's
  @racket[with-promises] type. For example,
  @racket[[City (Listof (List 'Street/with-indices-tag2 Index))
           (Listof (List 'Person/with-indices-tag2 Index))]].}
 @item{The @emph{with-promises} type, in which references to other node types
  must be replaced by a @racket[Promise] for the target node's
  @racket[with-promises] type. For example,
  @racket[[City (Listof (Promise Street/with-promises-type))
           (Listof (Promise Person/with-promises-type))]].}
 @item{The @emph{mapping function}, which takes some parameters and
  returns a node (using the code provided by the user)}]

We derive identifiers for these based on the @tc[node] name:

@;;;;
@chunk[<define-ids>
       (define-temp-ids "~a/make-placeholder" (node …) #:first-base root)
       (define-temp-ids "~a/placeholder-type" (node …))
       (define-temp-ids "~a/placeholder-tag" (node …))
       (define-temp-ids "~a/placeholder-queue" (node …))
       
       (define-temp-ids "~a/incomplete-type" (node …))
       (define-temp-ids "~a/make-incomplete" (node …))
       (define-temp-ids "~a/incomplete-tag" (node …))
       (define-temp-ids "~a/incomplete-type" ((field …) …))
       
       (define-temp-ids "~a/with-indices-type" (node …))
       (define-temp-ids "~a/make-with-indices" (node …))
       (define-temp-ids "~a/with-indices-tag" (node …))
       (define-temp-ids "~a/with-indices-tag2" (node …))
       (define-temp-ids "~a/with-indices-type" ((field …) …))
       
       (define-temp-ids "~a/with-promises-type" (node …))
       (define-temp-ids "~a/make-with-promises" (node …))
       (define-temp-ids "~a/with-promises-tag" (node …))
       (define-temp-ids "~a/with-promises-type" ((field …) …))
       
       (define-temp-ids "~a/mapping-function" (node …))]

@subsection{Overview}

The macro relies heavily on two sidekick modules: @tc[rewrite-type], and
@tc[fold-queue]. The former will allow us to derive from the ideal type of a
node the incomplete type and the with-promises type. It will also allow us to
search inside instances of incomplete nodes, in order to extract the
placehoders, and replace these parts with promises. The latter, @tc[fold-queue],
will be used to process all the pending placeholders, with the possibility to
enqueue more as new placeholders are discovered inside incomplete nodes.

When the graph constructor is called with the arguments for the root parameters,
it is equivalent to make and then resolve an initial placeholder. We will use a
function from the @tc[fold-queue] library to process the queues of pending
placeholders, starting with a queue containing only that root placeholder.
We will have one queue for each placeholder type.@note{It we had only one queue,
 we would have only one collection of results, and would need a @racket[cast]
 when extracting nodes from the collection of results.} The element types of the
queues will therefore be these placeholder types.

@chunk[<fold-queue-type-element>
       node/placeholder-type]

The return type for each queue will be the corresponding with-indices type. The
fold-queues function will therefore return a vector of with-indices nodes for
each node type.

@chunk[<fold-queue-type-result>
       node/with-indices-type]


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

The fold-queues macro takes a root element, in our case the root placeholder,
which it will insert into the first queue. The next clauses are the queue
handlers, which look like function definitions of the form
@tc[(queue-name [element : element-type] Δ-queues enqueue) : result-type]. The
@tc[enqueue] argument is a function used to enqueue elements and get a tag in
return, which can later be used to retrieve the processed element.

Since the @tc[enqueue] function is pure, it takes a parameter of the same type
as @tc[Δ-queues] representing the already-enqueued elements, and returns a
modified copy, in addition to the tag. The queue's processing body should return
two values: the result of processing the element, and the latest version of
@tc[Δ-queues], which stores the new elements to be added to the queue.

@chunk[<fold-queue>
       (fold-queues <root-placeholder>
                    [(node/placeholder-queue [e : <fold-queue-type-element>]
                                             Δ-queues
                                             enqueue)
                     : <fold-queue-type-result>
                     <fold-queue-body>]
                    ...)]

@subsection{Making placeholders for nodes}

We start creating the root placeholder which we provide to @tc[fold-queues].

@chunk[<root-placeholder>
       (root/make-placeholder root-expr …)]

To make the placeholder, we will need a @tc[node/make-placeholder] function for
each @tc[node]. We first define the type of each placeholder (a list of
arguments, tagged with the @tc[node]'s name):

@; TODO: maybe replace node types with placeholder types

@chunk[<define-placeholder-type>
       (define-type node/placeholder-type
         (List 'node/placeholder-tag
               param-type …))]

@; TODO: just use (variant [mapping param-type ...] ...)

Then we define the @tc[node/make-placeholder] function:

@chunk[<define-make-placeholder>
       (: node/make-placeholder (→ param-type … node/placeholder-type))
       (define (node/make-placeholder param …)
         (list 'node/placeholder-tag param …))]

@subsection{Making with-indices nodes}

We derive the @tc[with-indices] type from each @emph{ideal} node type using
the @tc[tmpl-replace-in-type] template metafunction from the rewrite-type
library. We replace all occurrences of a @tc[node] name with an @tc[Index],
which indicates at which index in the queue's results the successor can be
found.

@; TODO: use a type-expander here, instead of a template metafunction.

@CHUNK[<define-with-indices>
       (define-type field/with-indices-type
         (tmpl-replace-in-type field-type
                               [node (List 'node/with-indices-tag2 Index)]
                               …))
       …
       
       (define-type node/with-indices-type
         (List 'node/with-indices-tag field/with-indices-type …))
       
       (: node/make-with-indices (→ field/with-indices-type …
                                    node/with-indices-type))
       (define (node/make-with-indices field …)
         (list 'node/with-indices-tag field …))]

@subsection{Making with-promises nodes}

We derive the @tc[with-promises] type from each @emph{ideal} node type using
the @tc[tmpl-replace-in-type] template metafunction from the rewrite-type
library. We replace all occurrences of a @tc[node] name with a @tc[Promise] for
that node's @tc[with-promises] type.

@; TODO: use a type-expander here, instead of a template metafunction.

@CHUNK[<define-with-promises>
       (define-type field/with-promises-type
         (tmpl-replace-in-type field-type
                               [node (Promise node/with-promises-type)] …))
       …
       
       (define-type node/with-promises-type
         (List 'node/with-promises-tag
               field/with-promises-type …))
       
       (: node/make-with-promises (→ field/with-promises-type …
                                     node/with-promises-type))
       (define (node/make-with-promises field …)
         (list 'node/with-promises-tag field …))]

@subsection{Making incomplete nodes}

We derive the @tc[incomplete] type from each @emph{ideal} node type using
the @tc[tmpl-replace-in-type] template metafunction from the rewrite-type
library. We replace all occurrences of a @tc[node] name with its
@tc[placeholder] type.

@; TODO: use a type-expander here, instead of a template metafunction.

@CHUNK[<define-incomplete>
       (define-type field/incomplete-type
         (tmpl-replace-in-type field-type
                               [node node/placeholder-type] …))
       …
       
       (define-type node/incomplete-type
         (List 'node/incomplete-tag field/incomplete-type …))
       
       (: node/make-incomplete (→ field/incomplete-type … node/incomplete-type))
       (define (node/make-incomplete field …)
         (list 'node/incomplete-tag field …))]

@subsection{Converting incomplete nodes to with-indices ones}

@chunk[<placeholder→with-indices-function>
       (λ ([p : node/placeholder-type] [acc : Void])
         : (values (List 'node/with-indices-tag2 Index) Void)
         (% index new-Δ-queues = (enqueue 'node/placeholder-queue p Δ-queues)
            (values (list 'node/with-indices-tag2 index)
                    acc)))]

@chunk[<placeholder→with-indices-clause>
       [node/placeholder-type
        (List 'node/with-indices-tag2 Index)
        (λ (x) (and (pair? x) (eq? (car x) 'node/placeholder-tag)))
        <placeholder→with-indices-function>]]

@subsubsection{Processing the placeholders}

@; TODO: also allow returning a placeholder (which means we should then
@; process that placeholder in turn). The placeholder should return the
@; same node type, but can use a different mapping?
@; Or maybe we can do this from the ouside, using a wrapper macro?

@CHUNK[<fold-queue-body>
       (let ([mapping-result (apply node/mapping-function (cdr e))])
         (let ([f (tmpl-fold-instance (List <field-incomplete-type> …)
                                      Void
                                      <placeholder→with-indices-clause> …)])
           (let-values ([(r new-acc) (f (cdr mapping-result) (void))])
             (values (cons 'node/with-indices-tag r)
                     Δ-queues))))]

Where @tc[<field-incomplete-type>] is the field-type in which node types are
replaced by placeholder types.

@chunk[ <field-incomplete-type>
       (tmpl-replace-in-type field-type
                             [node node/placeholder-type] …)]

@section{The mapping functions}

We define the mapping functions as they are described by the user, with an
important change: Instead of returning an @emph{ideal} node type, we expect them
to return an @emph{incomplete} node type.

@chunk[<define-mapping-function>
       (: node/mapping-function (→ param-type … node/incomplete-type))
       (define node/mapping-function
         (let ([mapping node/make-placeholder]
               …
               [node node/make-incomplete]
               …)
           (λ ([param : param-type] …) : node/incomplete-type
             . mapping-body)))]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@comment[#|
 @subsection{Converting incomplete nodes to with-promises ones}
  
 @chunk[<convert-incomplete-to-with-promises>
 [node/incomplete-type
 node/with-promises-type
 (λ (x) (and (pair? x) (eq? (car x) 'node/incomplete-tag)))
 (λ ([x : node/incomplete-type] [acc : Void])
 <convert-incomplete-successor>)]]
 
 @chunk[<convert-placeholder-to-with-promises>
 [mapping/placeholder-type
 (tmpl-replace-in-type result-type [node node/with-promises-type] …)
 (λ (x) (and (pair? x) (eq? (car x) 'mapping/placeholder-tag)))
 (λ ([x : mapping/placeholder-type] [acc : Void])
 <convert-placeholder-successor>)]]
 
 @; TODO: this would be much simpler if we forced having only one mapping per
 @; node, and extended that with a macro.
 
 @chunk[<define-compatible-mappings>
 (define/with-syntax ((node/compatible-mappings ...) ...)
 (for/list ([x (in-syntax #'(node ...))])
 (multiassoc-syntax
 x
 #'([result-type . mapping]
 …))))]
 
 @chunk[<convert-incomplete-successor>
 (error (~a "Not implemented yet " x))]
 
 @chunk[<convert-placeholder-successor>
 (% index new-Δ-queues = (enqueue 'mapping/placeholder-tag x Δ-queues)
 (list 'mapping/placeholder-tag index)
 (error (~a "Not implemented yet " x)))]
 |#]

@section{Putting it all together}

@chunk[<make-graph-constructor>
       (define-syntax/parse <signature>
         <define-ids>
         ((λ (x) (pretty-write (syntax->datum x)) x)
          (template
           (let ()
             (begin <define-placeholder-type>) …
             (begin <define-make-placeholder>) …
             (begin <define-with-indices>) …
             (begin <define-with-promises>) …
             (begin <define-incomplete>) …
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
         
         ;(begin-for-syntax
         ;<multiassoc-syntax>)
         
         (provide make-graph)
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