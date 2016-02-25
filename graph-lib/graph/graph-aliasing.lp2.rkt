#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Type and constructor
 aliases in graph declarations}

@(table-of-contents)

@section{Introduction}

When declaring a graph, the names of its nodes and mappings
as well as those of the graph it is based on may collide. We
try here to provide reasonnable defaults indicating which
name should refer to what at each point.

@chunk[<example>
       (graph g-old
              [City [streets : (Listof Street)]]
              [Street [name : String]]
              mappings…)
       (pass g-old → g-new
             ([City [streets : (Listof Street)] [nstreets : Index]]
              [Street [name : String]])
             (m-city ([g g-old.City]) : City
                     (City g.streets (length g.streets))))]

Capitalization aside, the name @tc[city] could refer to seven different things:
@itemlist[
 @item{The @racket[g-old.city] type}
 @item{The incomplete @racket[g-new.city] type}
 @item{The placeholder @racket[g-new.city] type}
 @item{The @racket[g-new.city] type, but most of the time this one should be
  needed only outside the graph declaration}
 @item{The @racket[city] constructor, returning an incomplete node}
 @item{The @racket[city] mapping, returning a placeholder}
 @item{A @racket[city] field, but this one is not a problem, since it will
  always be accessed via @racket[some-node-instance.city]}
 @item{The return type of the @racket[city] mapping, as in @racket[~> city]}]

Furthermore, the @racket[city] constructor should accept several cases for
@tc[street], when giving the list of values for the @tc[streets] field:
@itemlist[
 @item{A placeholder for @racket[street]}
 @item{An incomplete @racket[street]}
 @item{A @racket[g-old.street], which it will convert to the new type using the
  implicit mapping}]

In the field types, we have one case:
@itemlist[
 @item{The @racket[street] name, for example, should refer
  to the with-promises type of the new graph @racket[g-new],
  once the graph is fully constructed. When declaring the incomplete type for
  @racket[city], @racket[street] will refer to the incomplete @racket[street],
  but this is happening behind the scenes, and shouldn't cause any ambiguity.}]

In the mapping declaration, we have four cases:
@itemlist[
 @item{Return type: this should obviously refer to the name
  of the new node type (incomplete or placeholder).}
 @item{Parameter type: it would be tempting to make node names there refer to
  old node types. However, passing around placeholders and incomplete nodes then
  becomes difficult.}
 @item{Body, used as a function: inside the body, a node name should refer to
  the constructor for the new node type, returning a placeholder.}
 @item{Body, used as a type. A case where this could happen is when declaring an
  inner function. The case isn't clear here, as the function could return an
  incomplete node, or a placeholder. The same goes for the parameter type, or
  the type of variables bound by let.}]

The type and constructor aspects are independant, as the
same identifier can be used both as a type and as a function
without ambiguity. It would however be more intuitive if the
return type of @tc[city] when used as a function was a
subtype of what @tc[city] expands to when used as a type, so
that @tc[(ann (city x y) city)] is always valid.

It is important to note that once constructed, incomplete nodes can be made
opaque without loss of functionality. In other words, we are free to use
@tc[(U incomplete placeholder)] as the type for all constructed nodes.

We could therefore use @tc[(U incomplete placeholder)] everywhere, except for
the parameters, where we need to access the old graph types, which may conflict
with the new ones.

When there is a single mapping for the node, we have, schematically:

@chunk[<single-mapping>
       (g-old.city → g-new.city
                   (begin …
                          (city x y)
                          (bar other-old-city)))]

Where @tc[bar] is the constructor for another node taking a city for one of its
fields (here it will auto-call the mapping).

@section{Conclusion}

The solution we propose is to use @tc[(U incomplete placeholder)] everywhere for
the types, and disambiguate the old types in the parameters with @tc[old.city].
When used as a function, the name should refer to the constructor. We do not
have to worry about calling auto-defined mappings, as the constructors should
accept the old nodes, and convert them behind the scenes.

The only problem that remains, is when a user-defined mapping has the same name
as a node name, and has more than one argument (or doesn't accept a single old
node). This is probably something we can cope with, simply rejecting programs
that trigger this case, and request that the programmer uses different names for
the mapping and node.

@subsection{Shadowing}

We want to shadow the old @tc[city] type everywhere inside the graph
declaration. Otherwise this will lead to inconsistencies, inside an inner
definition for example. We do not want however the name of the nodes to leak
outside as definitions, as this would conflict with other graphs having the same
node names.

@chunk[<*>
       (begin)]
