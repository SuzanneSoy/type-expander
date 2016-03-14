#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: Constructor}

@(table-of-contents)

@section[#:tag "ADT|introduction"]{Introduction}

We define variants (tagged unions), with the following constraints:

@itemlist[
 @item{Unions are anonymous: two different unions can contain the same tag, and
  there's no way to distinguish these two occurrences of the tag}
 @item{Callers can require an uninterned tag which inherits the interned tag, so
  that @racket[(constructor #:uninterned tag Number)] is a subtype of
  @racket[(constructor #:uninterned tag Number)], but not the reverse}
 @item{The tag can be followed by zero or more “fields”}
 @item{An instance of a variant only @racket[match]es with its constructor and
  the same number of fields, with exact matching on the tag for uninterned
  tags}]

See @url{https://github.com/andmkent/datatype/} for an existing module providing
Algebraic Data Types. The main difference with our library is that a given tag
(i.e. constructor) cannot be shared by multiple unions, as can be seen in the
example below where the second @tc[define-datatype] throws an error:

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
primitives allowing us to build instance, match against them
and express the type itself.

@chunk[<require-modules>
       (require "structure.lp2.rkt")
       (require "constructor.lp2.rkt")]

We then define @tc[tagged], which is a shorthand for
manipulating constructors which single value is a promise
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
structures as described in @secref{ADT|introduction}.

@chunk[<require-modules>
       (require "define-adt.lp2.rkt")]

Finally, we define a @tc[uniform-get] form, which can
operate on @tc[tagged] structures. We also wrap the plain 
@tc[structure] form so that it instead returns a tagged
structure, using a common tag for all plain structures. This
allows us to rely on the invariant that @tc[uniform-get]
always operates on data with the same shape (a constructor
which single value is a promise for a structure)@note{This
 avoids the risk of combinatorial explosion for the intput
 type of @racket[uniform-get], when accessing a deeply nested
 field: allowing 
 @racket[(U structure
            (constructor structure)
            (constructor (Promise structure)))]
 would result in a type of size @${n⁴}, with ${n} then depth
 of the accessed field.}

@chunk[<require-modules>
       (void)] @;(require "uniform-get.lp2.rkt")

@chunk[<*>
       (void)
       #;(begin
         (module main typed/racket
           <require-modules>
           (provide constructor
                    define-constructor
                    tagged
                    define-tagged
                    variant
                    define-variant
                    (rename-out [wrapped-structure structure])
                    uniform-get))
         
         (require 'main)
         (provide (all-from-out 'main)))]