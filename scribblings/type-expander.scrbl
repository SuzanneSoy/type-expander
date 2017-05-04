#lang scribble/manual

@title{Type expander library}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}

@defmodule[type-expander
           #:use-sources [(lib "type-expander/type-expander.hl.rkt")
                          (lib "type-expander/more-expanders.hl.rkt")]]

@require[racket/require
         scribble/example
         @for-syntax[racket/base]
         @for-label[type-expander
                    type-expander/expander
                    (subtract-in typed/racket/base
                                 type-expander
                                 type-expander/expander)
                    (only-in racket/base [... …])
                    (prefix-in racket/base: racket/base)
                    syntax/stx
                    racket/list
                    syntax/parse
                    syntax/parse/experimental/template
                    auto-syntax-e
                    debug-scopes]]

@(require (for-syntax syntax/strip-context
                      syntax/stx
                      racket/syntax))
@(define-syntax (orig stx)
   (syntax-case stx ()
     [(_ name ...)
      (with-syntax ([(prefixed ...)
                     (stx-map (λ (id) (format-id id "orig:~a" id))
                              #'(name ...))])
        #`(begin
            (module #,(syntax-local-introduce #'orig-module) .
              #,(strip-context
                 #'(racket/base
                    (require (for-label (only-meta-in 0 (only-in typed/racket
                                                                 name ...))))
                    (require scribble/manual)
                    (define prefixed @racket[name]) ...
                    (provide prefixed ...))))
            (require #,(syntax-local-introduce #''orig-module))))]))

@(orig
  class
  ;;
  define-type
  ;; TODO: add all-defined-out in prims.rkt
  ;; top-interaction.rkt
  :type
  :print-type
  :query-type/args
  :query-type/result
  ;; case-lambda.rkt
  case-lambda
  case-lambda:
  pcase-lambda:
  ;; (submod "prims-contract.rkt" forms)
  require/opaque-type 
  ;require-typed-struct-legacy
  require-typed-struct
  ;require/typed-legacy
  require/typed
  require/typed/provide
  require-typed-struct/provide
  cast
  make-predicate
  define-predicate
  ;; prims.rkt
  define-type-alias
  define-new-subtype
  define-typed-struct
  define-typed-struct/exec
  ann
  inst
  :
  define-struct:
  define-struct
  struct
  struct:
  λ:
  lambda:
  lambda
  λ
  define
  let
  let*
  letrec
  let-values
  letrec-values
  let/cc
  let/ec
  let:
  let*:
  letrec:
  let-values:
  letrec-values:
  let/cc:
  let/ec:
  for
  for/list
  for/vector
  for/hash
  for/hasheq
  for/hasheqv
  for/and
  for/or
  for/sum
  for/product
  for/lists
  for/first
  for/last
  for/fold
  for*
  for*/list
  for*/lists
  for*/vector
  for*/hash
  for*/hasheq
  for*/hasheqv
  for*/and
  for*/or
  for*/sum
  for*/product
  for*/first
  for*/last
  for*/fold
  for/set
  for*/set
  do
  do:
  with-handlers
  define-struct/exec:
  define-struct/exec)

@(define eval-factory
   (make-eval-factory (list 'typed/racket 'type-expander)))

This library is implemented using literate programming. The
implementation details are presented in the 
@other-doc['(lib
             "type-expander/scribblings/type-expander-implementation.scrbl")]
document.

It enhances @racketmodname[typed/racket] with 
@deftech[#:key "type expander"]{type expanders}, which are
special macros that can appear wherever a regular type is
usually expected, and must expand to a type. Type expanders
are to types what
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")
      #:key "match expander"]{
 match expanders} are to @racket[match] patterns.

It is based on 
@hyperlink[(string-append "https://github.com/racket/racket/compare/"
                          "master...takikawa:tr-type-expander")]{
 Asumu Takikawa's type expanders} (see also his
@hyperlink["https://github.com/racket/racket/pull/604"]{original pull request}).
Asumu Takikawa's work attempted to integrate type expanders
directly into Typed/Racket. This project instead implements
type expanders as a library, which does not need any changes
to the core Typed/Racket codebase. This shows the
extensibility of Typed/Racket thanks to macros, and could
serve as the basis for other projects which need to alter
how Typed/Racket handles types.

The input for a type expander is the syntax used to call
it, just as the input to a macro is the syntax used to call
it. The output should be a type, which can itself contain
type expanders.

This library works by shadowing the definitions of
@orig::, @orig:define, @orig:lambda @etc from 
@racketmodname[typed/racket] with versions which support
type expanders.

@section{@(hash-lang) and module languages based on
 @racketmodname[type-expander]}

@subsection{@(hash-lang) combining @racketmodname[type-expander] and
 @racketmodname[typed/racket]}

@defmodulelang[type-expander
               #:link-target? #f]{
 The @racket[#,(hash-lang) #,(racketmodname type-expander)] language works like
 @racket[#,(hash-lang) #,(racketmodname typed/racket)], but it initially imports
 the forms overridden by @racketmodname[type-expander], instead of importing
 the original identifiers defined by @racket[typed/racket].

 This language cannot be used as a module language, instead use
 @racketmodname[type-expander/lang] which provides the same bindings.}

@subsection{Module language combining @racketmodname[type-expander] and
 @racketmodname[typed/racket]}

@defmodulelang[type-expander/lang]{
 This language is equivalent to
 @racket[#,(hash-lang) #,(racketmodname type-expander)], but can also be used as
 a module language.}

@subsection{@(hash-lang) and module language combining
 @racketmodname[type-expander] and @racketmodname[typed/racket/base]}

@defmodulelang[type-expander/base]{
 This language is similar to @racketmodname[type-expander/lang], but it
 exports the identifiers from @racketmodname[typed/racket/base] instead of
 @racket[typed/racket].}


@section{Defining new type expanders}

@defform*[((define-type-expander (name stx) . body)
           (define-type-expander name transformer-function))
          #:grammar ([name Identifier]
                     [stx Identifier]
                     [transformer-function (expr/c (-> syntax? syntax?))])]{
 The @racket[define-type-expander] form binds 
 @racket[_name] to a type expander, which can be used in
 places where a type would normally be expected.

 For example, one could define the @racket[HomogeneousList]
 type expander, which accepts a type @racket[_t] and an
 integer @racket[_n], and produces a @racket[List] type with
 @racket[_n] elements, each of type @racket[_t]:

 @racketblock[
 (define-type-expander (HomogeneousList stx)
   (syntax-case stx ()
     [(_ t n)
      (number? (syntax-e #'n))
      (with-syntax ([(tᵢ ...) (stx-map (const #'t)
                                       (range (syntax-e #'n)))])
        #'(List tᵢ ...))]))]}

@subsection{Attaching type expanders to existing identifiers}

@defform[(patch-type-expander name transformer-function)
         #:grammar ([name Identifier]
                    [transformer-function (expr/c (-> syntax? syntax?))])]{
 This macro records in a global table that @racket[name] should behave
 according to the given @racket[transformer-function], when used as a type.

 It allows attaching type expanders to existing identifiers, without shadowing
 them. It is used for example to attach the type expanders for @racket[quote],
 @racket[quasiquote], @racket[syntax] and @racket[quasisyntax] which are
 described below, and also for the @racket[curry] type expander.}

@section{Using a type expander}

The @racket[HomogeneousList] type expander defined above could be
used in many of @racketmodname[typed/racket]'s forms.

@racketblock[
 (define-type three-ints (HomogeneousList 3 Integer))
 (define (incr3 [x : three-ints]) : HomogeneousList
   (map add1 x))
 (ann (incr3 '(1 2 3)) HomogeneousList)]

Type expanders can produce types which may contain other
uses of type expanders, much in the same way as macros can
expand to code calling other macros. The type expander can
also produce directly a call to another type expander, just
as a macro can expand to a call to another macro, without
any extra surrounding syntax.

@; TODO: examples

Contrarily to macros, if a call to a type expander is in the
first position of more arguments, then the nested call is
first expanded, and can produce the name of a second
expander which will use the outer arguments, or can simply
produce a polymorphic type which will be applied to the
arguments. More than two levels of nesting are possible.

@; TODO: examples with two levels and more.

@section{Debugging type expanders}

@defform*[[(debug-type-expander #t)
           (debug-type-expander #f)]]{
 The first form enables printing of debugging information while expanding
 types, and the second form disables that behaviour. Debugging information is
 not printed by default.

 Currently, when debugging information is enabled, the type expander prints at
 each step a human-readable representation of the syntax object it is about to
 expand, and once an expansion step finishes, it prints the original syntax
 object as well as its expanded form. The identifiers are adorned with
 superscripts indicating the scopes present on them. See the documentation for
 the debugging tool @racket[+scopes] for more details.}

@section{Compile-time aspects of type expanders}

@defmodule[type-expander/expander
           #:use-sources
           [(submod (lib "type-expander/type-expander.hl.rkt") expander)
            (submod (lib "type-expander/type-expander.hl.rkt") main)]]

@defproc[(expand-type [stx Type]) PlainType]{
 Fully expands the type @racket[stx], which may contain any
 number of calls to type expanders. If those calls result in
 more type expanders, those are expanded too.}

@defproc[(apply-type-expander [type-expander-stx Identifier] [stx Syntax])
         Type]{
 Produces the result of applying the type expander bound to
 @racket[type-expander-stx] to the syntax @racket[stx].
 Normally, the syntax @racket[stx] would be of the form 
 @racket[(type-expander-stx arg …)] (similar to a macro
 call) or simply @racket[type-expander-stx] (similar to an
 @tech[#:doc '(lib
               "scribblings/guide/guide.scrbl")]{identifier
  macro}). It is however possible to pass arbitrary syntax
 to the type expander, just as it is possible for macros
 (for example @racket[set!] calls 
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
  assignment transformer} macros with the syntax 
 @racket[(set! macro-name arg …)] as an argument).}

@defthing[prop:type-expander struct-type-property?]{
 A 
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
  structure type property} to identify structure types that
 act as @tech[#:key "type expander"]{type expanders} like
 the ones created by @racket[define-type-expander].

 The property value must be a procedure of arity 1 or an 
 @racket[exact-nonnegative-integer?] designating a field
 index within the structure which contains such a
 procedure.

 The procedure serves as a syntax transformer when
 expanding the use of a type expander. If the type expander
 was in the first position of a syntax list (i.e. it looks
 like a macro or function call), then the whole syntax list
 is passed as an argument. Otherwise, just the identifier is
 passed as an argument, exactly as what would be done when
 calling an 
 @tech[#:doc '(lib
               "scribblings/guide/guide.scrbl")]{identifier macro}. The
 procedure can support other use patterns if desired, so
 that it would be possible in principle to implement special
 type forms that behave in a way similar to 
 @secref["set__Transformers" #:doc '(lib "scribblings/guide/guide.scrbl")].}

@subsection{Syntax class for @racketid[:]}

@defidform[#:kind "syntax-parse syntax class"
           colon]{
 This library shadows the @orig:: identifier from 
 @racketmodname[typed/racket] with a new definition 
 @racket[:], adjusted to handle type expanders. Programs
 using the @racketmodname[type-expander] library will
 therefore use our version of @racket[:]. The @racket[:]
 identifier provided by this library is not 
 @racket[free-identifier=?] with the original @orig:: from 
 @racketmodname[typed/racket]. This has an impact when
 writing patterns for the @racketmodname[syntax/parse]
 library, as the two identifiers @racket[:] and @orig:: are
 not the same from the point of view of the 
 @racket[~literal] pattern.

 The @racket[colon] syntax class is provided 
 @racket[for-syntax] by this library, and can be used in 
 @racket[syntax-parse] patterns, using @racket[c:colon] for
 example. It matches both the original @orig:: and the new 
 @racket[:], but not other @racketid[:] identifiers.

 It can be used to write macros which expect either 
 @racketid[:] identifier.}

@subsection{Syntax classes for types}

@defidform[#:kind "syntax-parse syntax class"
           type]{
 Matches a type. For now, this is just an alias for @racket[expr], because types
 can contain arbitrary syntax thanks to type expanders.}

@defthing[stx-type/c flat-contract?]{
 Flat contract which recognises syntax objects representing types. For now,
 this is just an alias for @racket[syntax?], because types can contain
 arbitrary syntax thanks to type expanders.

 Future versions may implement this as a non-flat contract, in order to be
 able to check that in a macro's result, the syntax for a type is not used as
 an expression, and vice versa.}

@defidform[#:kind "syntax-parse syntax class"
           type-expand!]{
 Matches a type @racket[_t], and provides an attribute named @racket[expanded]
 which contains the result of @racket[(expand-type #'_t)]. For now, 
 @racket[type-expand] does not perform any check other than verifying that 
 @racket[_t] is an @racket[expr], because types can contain arbitrary syntax
 thanks to type expanders.}

@section{multi-id}

@; TODO: separate multi-id or type-expander into two packages, so that we can
@; write @racketmodname[multi-id] without causing a circular dependency:
Type expanders are supported by the multi-id library. It is
therefore easy to define an identifier which acts as a type
expander and match expander as well as a regular racket
macro and/or 
@tech[#:doc '(lib
              "scribblings/guide/guide.scrbl")]{identifier macro}. This
can be useful to define feature-rich data structures, which
need to provide all of the above features.

@section{Expansion model for type expanders}

The expansion model for type expanders is similar to the expansion model for
macros. There are a few differences, however, which are presented below.

@itemlist[
 @item{When a form like @racket[(f args ... . rest)] is encountered, if its
  first element, @racket[f], is a type expander, the type expander is applied to
  the whole form. If @racket[f] is a special identifier (e.g. like @racket[Let]
  or @racket[Rec]), then the form is handled according to the special
  identifier's rules. Otherwise, the @racket[f] form is expanded, and the result
  @racket[(e args ... . rest)] is expanded once again (@racket[e] being the
  result of the expansion of @racket[f]).

  In comparison, the ``official'' macro expander for Racket would, in the last
  case, expand @racket[f] on its own, and then expand the arguments one by one
  without re-considering the form as a whole.

  
  With the type expander, during the second expansion pass for the form, if the
  @racket[e] identifier is a type expander it is applied to the whole form. If
  @racket[e] is a special identifier, the form is processed following that
  identifier's rules. Otherwise, the @racket[e] form is left intact, and the
  arguments @racket[args ...] and @racket[rest] are expanded each in turn.

  In comparison, the ``official'' macro expander would have fully expanded
  @racket[e] in isolation (e.g. as an identifier macro), without letting it take
  over the arguments.}
 @item{With the ``official'' macro expander, all forms at the same lexical
  scoping level are expanded before expanding the contents of @racket[let]
  forms.

  In contrast, the type expander expands the contents of @racket[Let] forms in
  the same order as other forms. It further replaces the @racket[Let] forms by
  their contents, so that the following type:

  @racketblock[((Let ([Foo Pairof]) Foo) Number String)]

  gets expanded by replacing @racket[(Let ([Foo Pairof]) Foo)] by its contents
  (i.e. the @racket[Foo] identifier in this case):

  @racketblock[(Foo Number String)]

  The @racket[Foo] identifier is still bound to @racket[Pairof], so this new
  type expression gets expanded to:

  @racketblock[(Pairof Number String)]

  This means that identifiers bound by @racket[Let] forms can escape their
  scope, but are still attached to their defining scope.}
 @item{With the current implementation of the type expander,
  @racket[syntax-local-value] ignores types bound by @racket[Let] forms. A
  future version of this library will (hopefully) either fix this problem, or
  provide an alternative @racket[syntax-local-type-value] which takes those
  bindings into account.}]

@section{Built-in type expanders}

There are several built-in expanders. Some are documented
here, while others are listed in
@secref["Cases_handled_by_expand-type"
        #:doc '(lib "type-expander/type-expander.hl.rkt")].
Their API should be considered unstable, and may change in
the future.

@subsection{Let}

@defform[#:kind "type expander"
         (Let ([Vᵢ Eᵢ] …) τ)
         #:grammar
         ([Vᵢ Identifier]
          [Eᵢ Type]
          [τ Type])]{
 The @racket[Let] form binds each type expression 
 @racket[Eᵢ] (which may contain uses of type expanders bound
 outside of the @racket[Let] form) to the identifier @racket[Vᵢ].
 The type @racket[τ] can contain type expanders and can
 refer to occurrences of the bound @racket[Vᵢ] identifiers,
 which will expand to @racket[Eᵢ]. The @racket[Let] form therefore
 behaves is a way similar to @racket[let-syntax].

 @examples[#:eval (eval-factory)
           (ann '(1 2 3)
                (Let ([Foo Number])
                  (Listof Foo)))
           (eval:error (ann '(1 2 3)
                            (Listof Foo)))]

 @examples[#:eval (eval-factory)
           (ann '([1 . "a"] [2 . b] [3 . 2.71])
                (Let ([Foo (Λ (_ T)
                             #'(Pairof Number T))])
                  (List (Foo String)
                        (Foo Symbol)
                        (Foo Float))))]

@examples[#:eval (eval-factory)
          (ann '(a b c)
               (Let ([Foo Number])
                 (Let ([Foo String])
                   (Let ([Foo Symbol])
                     (Listof Foo)))))
          (ann '(a b c)
               (Let ([Foo Number])
                 (Listof (Let ([Foo String])
                           (Let ([Foo Symbol])
                             Foo)))))]}

@subsection{Letrec}

@defform[#:kind "type expander"
         (Letrec ([Vᵢ Eᵢ] …) τ)]{
 Like @racket[Let], but all the @racket[Vᵢ] identifiers are bound within all
 the @racket[Eᵢ] type expressions. This means the type expression within an
 @racket[Eᵢ] can refer to any @racket[Vᵢ] of the same @racket[Letrec].}


@subsection{Let*}

@defform[#:kind "type expander"
         (Let* ([Vᵢ Eᵢ] …) τ)]{
 Like @racket[Let], but all the preceding @racket[Vᵢ] identifiers are bound
 each @racket[Eᵢ] type expression. This means the type expression within an
 @racket[Eᵢ] can refer to any @racket[Vᵢ] already bound above it, but not to
 the @racket[Vᵢ] it is being bound to, nor to the following @racket[Vᵢ].}

@subsection{Λ}

@defform[#:kind "type expander"
         (Λ formals . body)
         #:grammar
         ([stx Identifier])]{
                             
 The @racket[Λ] form (a capital @racketid[λ]) can be used to construct an
 anonymous type expander. It is equivalent to replacing the whole
 @racket[(Λ formals . body)] form with @racket[_generated-id], where
 @racket[_generated-id] is defined as a named type expander as follows:

 @racketblock[(define-type-expander (_gen-id _gen-stx-id)
                (auto-syntax-case _gen-stx-id ()
                  [formals (let () . body)]))]

 where @racket[_id] and @racket[_gen-stx-id] are fresh unique identifiers.

 Since @racket[Λ] relies on @racket[auto-syntax-case], the syntax pattern
 variables bound by @racket[formals] can also be used outside of syntax
 templates, in which case they evaluate to @racket[(syntax->datum #'pvar)].

 @examples[#:eval (eval-factory)
           #:escape UNSYNTAX
           (eval:no-prompt (require (for-syntax racket/list racket/function)))
           (ann '(1 2 3 4)
                ((Λ (_ T n)
                   #`(List #,@(map (const #'T) (range n))))
                 Number 4))]}

@subsection{Quasiquote}

The type expander library also adds support for
quasiquoting in types: The type @racket[`(a (1 b) ,String)]
is expanded to @racket[(List 'a (List 1 'b) String)].

@examples[#:eval (eval-factory)
          (ann '(a (1 b) "foo")
               `(a (1 b) ,String))]

The @racket[quote], @racket[quasiquote], @racket[syntax] and
@racket[quasisyntax] identifiers are interpreted specially within type
expressions. The @racket[quote] identifier can be used to describe a type
matching containing only the quoted value. Similarly, @racket[syntax] can be
used to describe the type of the quoted syntax object, without the need to
insert @racket[Syntaxof] by hand around each part of the type. Note that the
type @racket[#'(a b c)] will match the syntax object @racket[#'(a b c)], but
not the syntax object @tt{#'(a b . (c))}, i.e. the generated type is
sensitive to the distinction between syntax pairs and syntax lists. It is
possible that a future version of this library provides another type expander
which accepts both. The @racket[quasiquote] and @racket[quasisyntax] forms
allow the use of @racket[unquote] and @racket[unsyntax], respectively.

@subsection{Currying type expanders}

The @racket[curry] special type-expander form can be used to curry in some
arguments to a type expander.

@examples[#:eval (eval-factory)
          (ann '([a . 1] [a . b] [a . "c"])
               (Let ([PA (curry Pairof 'a)])
                 (List (PA 1) (PA 'b) (PA "c"))))]

@section{Common issues (FAQ)}

@(require (only-in scribble/eval interaction))
@itemlist[
 @item{Explicitly requiring @racketmodname[typed/racket]
  causes an error:
  @(let ([errmsg (string-append "module: identifier already imported from"
                                " a different source in:" "\n"
                                "  λ:" "\n"
                                "  type-expander" "\n"
                                "  typed/racket" "\n")])
     @interaction[(eval:alts (require typed/racket type-expander)
                             (eval:result ""
                                          ""
                                          errmsg))])
  A required module can shadow the definitions provided by
  the @litchar{#lang} language, but it cannot shadow the
  definitions provided by other explicitly required
  modules.

  The solution is to avoid explicitly requiring 
  @racketmodname[typed/racket], or to subtract from it the
  identifiers that would otherwise be shadowed anyway:

  @racketblock[
 (require racket/require
          (subtract-in typed/racket type-expander)
          type-expander)]}
 @item{An error complains that a type expander is unbound:
  @(let ([errmsg (string-append "Type Checker: parse error in type;\n"
                                " type name `foo' is unbound")])
     @interaction[(eval:alts (module main typed/racket
                               (module m type-expander/lang
                                 (provide foo)
                                 (define-type-expander (foo stx) #'Void))
                               (require 'm)
                               (: v foo)
                               (define v (void)))
                             (eval:result ""
                                          ""
                                          errmsg))])

  This error will be raised if the @racketmodname[type-expander] library is not
  @racket[require]d. It is best to double-check that a
  @racket[(require type-expander)] form is present, and that it is present at
  the appropriate meta-level (it should be loaded at the same meta-level as the
  use of @racket[(: var type)], @racket[(define var : type value)]).
  
  In the example above, the problem is that the module @racketid[main] requires
  @racket['m], but does not require @racketmodname[type-expander]. The @orig::
  in @racket[(#,orig:: #,(racketid v) #,(racketid foo))] therefore comes from
  @racketmodname[typed/racket], and does not know how to use the @racketid[foo]
  type expander.}
 @item{@bold{Q:} Can I write a recursive type-level
  function?

  @bold{A:} Yes, but be sure that it is not infinitely
  recursive, as the expansion would never terminate, unlike
  @racketmodname[typed/racket]'s @racket[Rec], which allows
  truly recursive types.

  Furthermore, it is best to ponder the risk of
  combinatorial explosion, for example in 
  @racketmodname[typed/racket], 
  @racket[((∀ (X) (List X X)) Number)] expands internally to
  the type @racket[(List Number Number)]. Nesting this
  pattern a few times will produce a type having an
  in-memory representation whose size is exponential in the
  size of the original type declaration. A type expander can
  easily produce a very large type, which will bring the
  type checker to a crawl and/or crash it.}]

@section{Overloaded @racketmodname[typed/racket] primitives}


@defform[(unsafe-cast value type)]{
 We define an @racket[unsafe-cast] form which is not (yet) provided by
 Typed/Racket. It works like @racket[cast], but does not generate a predicate
 to check that the value is indeed of the given type. It can therefore be used
 to cast values to types for which @racket[cast] would fail at compile-time
 when trying to generate the predicate, for example function types, or any type
 which translates to a
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{chaperone}
 contract.}

@defform[(unsafe-cast/no-expand value type)]{
 Like @racket[unsafe-cast], but does not expand the type. Can be useful for
 types which are not completely handled by @racketmodname[type-expander], for
 example function types with filters.}

@(require (for-syntax racket/function racket/struct racket/vector))
@(define-for-syntax (strip-loc e)
   (cond [(syntax? e) (datum->syntax e (strip-loc (syntax-e e)) #f)]
         [(pair? e) (cons (strip-loc (car e)) (strip-loc (cdr e)))]
         [(vector? e) (vector-map strip-loc e)]
         [(box? e) (box (strip-loc (unbox e)))]
         [(prefab-struct-key e)
          => (λ (k) (apply make-prefab-struct
                           k
                           (strip-loc (struct->list e))))]
         [else e]))

@(define-syntax (ovl stx)
   (syntax-case stx ()
     [(_ name ...)
      (with-syntax ([(prefixed ...)
                     (stx-map (λ (id) (format-id id "orig:~a" id))
                              #'(name ...))]
                    [(stripped-name ...)
                     (stx-map strip-loc
                              #'(name ...))]
                    [(stripped-ooo ...)
                     (stx-map (compose strip-loc stx-car stx-cdr)
                              #'([name (... ...)] ...))])
        #'(list
           @defform[(stripped-name stripped-ooo)]{
       Overloaded version of @|prefixed| from 
       @racketmodname[typed/racket].}
           ...))]))

@ovl[
 :
 :type
 :print-type
 :query-type/args
 :query-type/result
 define-type
 define
 lambda
 λ
 case-lambda
 case-lambda:
 struct
 define-struct/exec
 ann
 cast
 inst
 let
 let*
 let-values
 make-predicate
 ;;
 class]

@defidform[...*]{
 Overloaded version of @racketid[...*], which is interpreted specially by
 @racketmodname[typed/racket]. It seems to be equivalent to @racket[*] for
 indicating the type of a rest argument within a typed @orig:λ form.}

@section{Unimplemented @racketmodname[typed/racket]
 primitives (will be overloaded in later versions).}

@(define-syntax (ovl-todo stx)
   (syntax-case stx ()
     [(_ name ...)
      (with-syntax ([(prefixed ...)
                     (stx-map (λ (id) (format-id id "orig:~a" id))
                              #'(name ...))]
                    [(stripped-name ...)
                     (stx-map strip-loc
                              #'(name ...))]
                    [(stripped-ooo ...)
                     (stx-map (compose strip-loc stx-car stx-cdr)
                              #'([name (... ...)] ...))])
        #'(list
           @defform[(stripped-name stripped-ooo)]{
       Overloaded version of @|prefixed| from
       @racketmodname[typed/racket] (not implemented for the
       @racketmodname[type-expander] library yet, just throws an
       error).}
           ...))]))

@ovl-todo[
 ;; TODO: add all-defined-out in prims.rkt
 ;; top-interaction.rkt
 ;; case-lambda.rkt
 pcase-lambda:
 ;; (submod "prims-contract.rkt" forms)
 require/opaque-type 
 ;require-typed-struct-legacy
 require-typed-struct
 ;require/typed-legacy
 require/typed
 require/typed/provide
 require-typed-struct/provide
 ;cast
 define-predicate
 ;; prims.rkt
 define-type-alias
 define-new-subtype
 define-typed-struct
 define-typed-struct/exec
 define-struct:
 define-struct
 struct:
 λ:
 lambda:
 letrec
 letrec-values
 let/cc
 let/ec
 let:
 let*:
 letrec:
 let-values:
 letrec-values:
 let/cc:
 let/ec:
 for
 for/list
 for/vector
 for/hash
 for/hasheq
 for/hasheqv
 for/and
 for/or
 for/sum
 for/product
 for/lists
 for/first
 for/last
 for/fold
 for*
 for*/list
 for*/lists
 for*/vector
 for*/hash
 for*/hasheq
 for*/hasheqv
 for*/and
 for*/or
 for*/sum
 for*/product
 for*/first
 for*/last
 for*/fold
 for/set
 for*/set
 do
 do:
 with-handlers
 define-struct/exec:]

@include-section{deprecated-colon.scrbl}

@include-section{contracts-to-types.scrbl}