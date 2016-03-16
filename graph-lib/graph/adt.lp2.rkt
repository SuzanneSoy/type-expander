#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: Constructor}

@(table-of-contents)

@section[#:tag "ADT|introduction"]{Introduction}

We define variants (tagged unions), with the following constraints:

@; TODO: put a short usage example here

@itemlist[
 @item{Unions are anonymous: two different unions can
  contain the same tag, and there's no way to distinguish
  these two occurrences of the tag}
 @item{Users can define an uninterned tag, i.e. one which
  will not match against other uses of the same tag name. A
  constructor using this uninterned tag is a subtype of the
  constructor using the interned one, but not the reverse.
  This means that 
  @racket[(constructor #:uninterned tag Number)] is a
  subtype of @racket[(constructor tag Number)], but not the
  opposite. This allows testing if a constructor instance
  was created by the rightful owner, or by some other code
  which happens to use the same name.}
 @item{The tag can be followed by zero or more “fields”}
 @item{An instance of a variant only @racket[match]es with
  its constructor and the same number of fields, with exact
  matching on the tag for uninterned tags}]

See @url{https://github.com/andmkent/datatype/} for an
existing module providing Algebraic Data Types. The main
difference is that unlike our library, a given constructor
name cannot be shared by multiple unions, as can be seen in
the example below where the second @tc[define-datatype]
throws an error:

@chunk[<datatype-no-sharing>
       (require datatype)
       
       (define-datatype Expr
         [Var (Symbol)]
         [Lambda (Symbol Expr)]
         [App (Expr Expr)])
       
       ;; define-datatype: variant type #<syntax:11:3 Var> already bound
       ;; in: Simple-Expr
       (define-datatype Simple-Expr
         [Var (Symbol)]
         [Lambda (Symbol Expr)])]

@section{Constructors, tagged, variants and structures}

We first define @tc[structure] and @tc[constructor], the
primitives allowing us to build instances, match against them
and express the type itself.

@chunk[<require-modules>
       (require "structure.lp2.rkt")
       (require "constructor.lp2.rkt")]

We then define @tc[tagged], which is a shorthand for
manipulating constructors whose single value is a promise
for a structure.

@chunk[<require-modules>
       (require "tagged.lp2.rkt")]

For convenience, we write a @tc[variant] form, which is a
thin wrapper against @tc[(U (~or constructor tagged) …)].

@chunk[<require-modules>
       (require "variant2.lp2.rkt")]

The @tc[define-tagged] and @tc[define-constructor] forms
also allow the @tc[#:uninterned] and @tc[#:private]
keywords, to create uninterned constructors and tagged
structures as described in the @secref{ADT|introduction}.

@chunk[<require-modules>
       (require "define-adt.lp2.rkt")]

Finally, we define a @tc[uniform-get] form, which can
operate on @tc[tagged] structures. We also wrap the plain 
@tc[structure] form so that it instead returns a tagged
structure, using a common tag for all plain structures. This
allows us to rely on the invariant that @tc[uniform-get]
always operates on data with the same shape (a constructor
whose single value is a promise for a structure)@note{This
 avoids the risk of combinatorial explosion for the intput
 type of @racket[uniform-get], when accessing a deeply nested
 field: allowing
 @racket[(U structure
            (constructor structure)
            (constructor (Promise structure)))]
 would result in a type of size @${n⁴}, with @${n} the depth
 of the accessed field.}

@chunk[<require-modules>
       (require "uniform-get.lp2.rkt")]

@chunk[<*>
       (begin
         (module main typed/racket
           <require-modules>
           (provide constructor
                    define-constructor
                    ConstructorTop
                    ConstructorTop?
                    constructor?
                    constructor-values
                    tagged
                    define-tagged
                    variant
                    define-variant
                    (rename-out
                     [wrapped-structure structure]
                     [wrapped-structure-supertype structure-supertype]
                     [structure plain-structure]
                     [structure-supertype plain-structure-supertype]
                     [define-structure define-plain-structure])
                    uniform-get))
         
         (require 'main)
         (provide (all-from-out 'main)))]