#lang hyper-literate racket/base #:no-require-lang
@; The #:no-require-lang above is needed because type-expander requires
@; from 'main some identifiers (e.g. λ) which conflict with the re-required
@; racket/base. With this option, we loose arrows in DrRacket for the
@; built-ins in this file, and have otherwise no adverse effects.
@(require scribble-enhanced/doc)
@doc-lib-setup

@(module orig-ids racket/base
   (require scribble/manual
            (for-label typed/racket/base))
   (provide (all-defined-out))
   (define orig:: (racket :))
   (define orig:let (racket let))
   (define orig:→AnyBoolean:Integer (racket (→ Any Boolean : Integer))))
@(require 'orig-ids)

@(unless-preexpanding
  (require racket/require
           (for-label (submod "..")
                      (only-in (submod ".." main) colon)
                      (subtract-in typed/racket/base (submod ".."))
                      (subtract-in racket typed/racket/base (submod ".."))
                      racket/require-syntax
                      racket/provide-syntax
                      typed/racket/unsafe
                      racket/format
                      racket/syntax
                      syntax/stx
                      syntax/parse
                      syntax/parse/experimental/template
                      syntax/id-table
                      auto-syntax-e
                      #;(subtract-in typed-racket/base-env/annotate-classes
                                     (submod "..")))))

@title[#:style manual-doc-style
       #:tag "ty-xp-impl"
       #:tag-prefix "type-expander/ty-xp-impl"
       ]{Implementation of the type expander library}

@(chunks-toc-prefix
  '("(lib type-expander/scribblings/type-expander-implementation.scrbl)"
    "type-expander/ty-xp-impl"))

This document describes the implementation of the 
@racketmodname[type-expander] library, using literate
programming. For the library's documentation, see the 
@other-doc['(lib "type-expander/scribblings/type-expander.scrbl")]
document instead.

@section{Introduction}

Extensible types would be a nice feature for typed/racket. Unlike
@racket[require] and @racket[provide], which come with
@tc[define-require-syntax] and @tc[define-provide-syntax], and unlike
@tc[match], which comes with @tc[define-match-expander], @tc[typed/racket]
doesn't provide a way to define type expanders. The
@racketmodname[type-expander] library extends @racketmodname[typed/racket]
with the ability to define type expanders, i.e. type-level macros.

The @secref["ty-xp-more" #:tag-prefixes '("type-expander/ty-xp-more")] section
presents a small library of type expanders built upon the mechanism implemented
here.

We redefine the forms @tc[:], @tc[define], @tc[lambda] and so on to
equivalents that support type expanders. Type expanders are defined via the
@tc[define-type-expander] macro. Ideally, this would be handled directly by
@tc[typed/racket], which would directly expand uses of type expanders.

@(table-of-contents)

@section{Expansion model for type expanders}

Type expanders are expanded similarly to macros, with two minor differences:
@itemlist[
 @item{A form whose first element is a type expander, e.g.
  @racket[(F . args₁)], can expand to the identifier of another type expander
  @racket[G]. If the form itself appears as the first element of an outer form,
  e.g. @racket[((F . args₁) . args₂)], the first expansion step will result in
  @racket[(G . args₂)]. The official macro expander for Racket would then expand
  @racket[G] on its own, as an
  @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro}, without
  passing the @racket[args₂] to it. In contrast, the type expander will expand
  the whole @racket[(G . args₂)] form, letting @racket[G] manipulate the
  @racket[args₂] arguments.}
 @item{It is possible to write anonymous macros, 

  The @racket[Λ] form can be used to create anonymous type expanders. Anonymous
  type expanders are to type expanders what anonymous functions are to function
  definitions. The following table presents the expression-level and type-level
  function and macro forms. Note that @racket[Let] serves as a type-level
  equivalent to both @racket[let] and @racket[let-syntax], as anonymous macros
  can be used in conjunction with @racket[Let] to obtain the equivalent of
  @racket[let-syntax].

  @tabular[#:style 'boxed
           #:sep (hspace 1)
           #:column-properties '((right-border right) left)
           #:row-properties '((bottom-border baseline) (baseline))
           (list (list ""
                       @bold{Definitions}
                       @bold{Local binding}
                       @bold{Anonymous functions})
                 (list @bold{Functions}
                       @racket[define]
                       @racket[let]
                       @racket[λ])
                 (list @bold{Macros}
                       @racket[define-syntax]
                       @racket[let-syntax]
                       @emph{N/A})
                 (list @bold{Type‑level functions@superscript{a}}
                       @racket[define-type]
                       @racket[Let]
                       @racket[∀])
                 (list @bold{Type‑level macros}
                       @racket[define-type-expander]
                       @racket[Let]
                       @racket[Λ]))]
  
  @superscript{a}: The type-level functions are simple substitution functions,
  and cannot perform any kind of computation. They are, in a sense, closer to
  pattern macros defined with @racket[define-syntax-rule] than to actual
  functions.}]

Combined, these features allow some form of "curried" application of type
expanders: The @racket[F] type expander could expand to an anonymous
@racket[Λ] type expander which captures the @racket[args₁] arguments. In the
second expansion step, the @racket[Λ] anonymous type expander would then
consume the @racket[args₂] arguments, allowing @racket[F] to effectively
rewrite the two nested forms, instead of being constrained to the innermost
form.

@subsection{Comparison with TeX's macro expansion model}

For long-time TeX or LaTeX users, this may raise some concerns. TeX programs
are parsed as a stream of tokens. A TeX commands is a macro. When a TeX macro
occurs in the stream of tokens, it takes its arguments by consuming a certain
number of tokens following it. After consuming these arguments, a TeX macro
may expand to another TeX macro, which in turn consumes more arguments. This
feature is commonly used in TeX to implement macros which consume a variable
number arguments: the macro will initially consume a single argument.
Depending on the value of that argument, it will then expand to a macro taking
@racket[_n] arguments, or another macro taking @racket[_m] arguments. This
pattern, omnipresent in any sufficiently large TeX program, opens the door to
an undesirable class of bugs: when a TeX macro invocation appears in the
source code, it is not clear syntactically how many arguments it will
eventually consume. An incorrect parameter value can easily cause it to
consume more arguments than expected. This makes it possible for the macro to
consume the end markers of surrounding environments, for example in the code:

@verbatim|{
                                                        \begin{someEnvironment}
                                                        \someMacro{arg1}{arg2}
                                                        \end{someEnvironment}
                                                        }|

the @literal|{someMacro}| command may actually expect three arguments, in
which case it will consume the @literal|{\end}| token, but leave the
@literal|{{someEnvironment}}| token in the stream. This will result in a badly
broken TeX program, which will most likely throw an error complaining that the
environment @literal|{\begin{someEnvironment}}| is not properly closed. The
error may however occur in a completely different location, and may easily
cause a cascade of errors (the missing @literal|{\end{someEnvironment}}| may
cause later TeX commands to be interpreted in a different way, causing them to
misinterpret their arguments, which in turn may cause further errors. The end
result is a series of mysterious error messages somewhat unrelated to the
initial problem.

This problem with TeX macros can be summed up as follows: the number of tokens
following a TeX macro invocation that will be consumed by the macro is
unbounded, and cannot be easily guessed by looking at the raw source code,
despite the presence of programmer-friendly looking syntactic hints, like
wrapping arguments with @literal|{{…}}|.

We argue that the expansion model for type expanders is less prone to this
class of problems, for several reasons:
@itemlist[
 @item{Firstly, macros can only consume outer forms if they appear as the
  leftmost leaf of the outer form, i.e. while the @racket[F] macro in the
  expression

  @racketblock[((F . args₁) . args₂)]

  may access the @racket[args₂] arguments, it will be constrained within the
  @racket[(F . args₁)] in the following code:

  @racketblock[(H leading-args₂ (F . args₁) . more-args₂)]

  The first case occurs much more rarely than the second, so is less likely to
  happen}
 @item{Secondly, all TeX macros will consume an arbitrary number of arguments
  in a linear fashion until the end of the enclosing group or a paragraph
  separation. In contrast, most type expanders will consume all the arguments
  within their enclosing application form, and no more. ``Curried'' type
  expanders, which expand to a lone macro identifier, will likely only represent
  a small subset of all type expanders. For comparison, consider the following
  TeX code:

  @verbatim|{\CommandOne{argA}\CommandTwo{argB}}|

  The @literal|{\CommandOne}| TeX macro might consume zero, one, two three or
  more arguments. If it consumes zero arguments, @literal|{{argA}}| will not be
  interpreted as an argument, but instead will represent a scoped expression,
  similar to @racket[(let () argA)]. If @literal|{\CommandOne}| consumes two or
  more arguments, @literal|{\CommandTwo}| will be passed as an argument,
  unevaluated, and may be discarded or applied to other arguments than the
  seemingly obvious @literal|{{argB}}| argument. The TeX code above could
  therefore be equivalent to any the following Racket programs:

  @racketblock[
 (CommandOne)
 (let () argA)
 (CommandTwo)
 (let () argB)]

  @racketblock[
 (CommandOne argA)
 (CommandTwo)
 (let () argB)]

  @racketblock[
 (CommandOne)
 (let () argA)
 (CommandTwo argB)]

  @racketblock[
 (CommandOne argA)
 (CommandTwo argB)]

  @racketblock[
 (CommandOne argA CommandTwo)
 (let () argB)]

  @racketblock[
 (CommandOne argA CommandTwo argB)]

  In contrast, the obvious interpretation at a first glance of the TeX program
  would be written as follows in Racket:

  @racketblock[
 (CommandOne argA)
 (CommandTwo argB)]

  If these appear as ``arguments'' of a larger expression, then their meaning
  is unambiguous (unless the larger expression is itself a macro):

  @racketblock[
 (+ (CommandOne argA)
    (CommandTwo argB))]

  If however the @racket[(CommandOne argA)] is the first element in its form,
  then, if it is a curried macro, it may consume the the
  @racket[(CommandTwo argB)] form too:

  
  @racketblock[
 ((CommandOne argA)
  (CommandTwo argB))]

  As stated earlier, this case will likely be less common, and it is clearer
  that the intent of the programmer to pass @racket[(CommandTwo argB)] as
  arguments to the result of @racket[(CommandOne argA)], either as a macro
  application or as a regular run-time function application.}
 @item{Finally, Racket macros (and type expanders) usually perform a somewhat
  thorough check of their arguments, using @racket[syntax-parse] or
  @racket[syntax-case] patterns. Arguments to macros and type expanders which do
  not have the correct shape will trigger an error early, thereby limiting the
  risk of causing errors in cascade.}]

@subsection{Interactions between type expanders and scopes}

Our expansion model for type expanders therefore allows a type expander to
escape the scope in which it was defined before it is actually expanded. For
example, the following type:

@RACKETBLOCK[
 (Let ([A Number])
   ((Let ([F (Λ (self T)
                #`(Pairof #,(datum->syntax #'self 'A)
                          T))])
      (Let ([A String])
        F))
    A))]

first expands to:

@RACKETBLOCK[
 (F
  A)]

and then expands to:

@RACKETBLOCK[
 (Pairof String A)]

and finally expands to:

@RACKETBLOCK[
 (Pairof String A)]

Effectively, @racket[F] captures the scope where its name appears (inside all
three @racket[Let] forms), but is expanded in a different context (outside of
the two innermost @racket[Let] forms).

Using Matthew Flatt's notation to indicate the scopes present on an
identifier, we can more explicitly show the expansion steps:

@RACKETBLOCK[
 (Let ([A Number])
   ((Let ([F (Λ (self T)
                #`(Pairof #,(datum->syntax #'self 'A)
                          T))])
      (Let ([A String])
        F))
    A))]

The first @racket[Let] form annotates the identifier it binds with a fresh
scope, numbered @racket[1] here, and adds this scope to all identifiers within
its body. It stores the binding in the @racket[(tl-redirections)] binding
table, as shown by the comment above the code

@RACKETBLOCK[
 (code:comment "A¹   := Number")
 ((Let¹ ([F¹ (Λ¹ (self¹ T¹)
                 #`(Pairof¹ #,(datum->syntax¹ #'self¹ 'A¹)
                            T¹))])
        (Let¹ ([A¹ String¹])
              F¹))
  A¹)]

The second @racket[Let] form then binds the @racket[F] identifier, adding a
fresh scope as before:

@RACKETBLOCK[
 (code:comment "A¹   := Number")
 (code:comment "F¹²  := (Λ¹ (self¹ T¹)")
 (code:comment "            #`(Pairof¹ #,(datum->syntax¹ #'self¹ 'A¹)")
 (code:comment "                       T¹))")
 ((Let¹² ([A¹² String¹²])
         F¹²)
  A¹)]

The third @racket[Let] form then binds @racket[A] within its body, leaving the
outer @racket[A] unchanged:

@RACKETBLOCK[
 (code:comment "A¹   := Number")
 (code:comment "F¹²  := (Λ¹ (self¹ T¹)")
 (code:comment "            #`(Pairof¹ #,(datum->syntax¹ #'self¹ 'A¹)")
 (code:comment "                       T¹))")
 (code:comment "A¹²³ := String¹²")
 (F¹²³
  A¹)]

The @racket[F¹²³] macro is then expanded, passing as an argument the syntax
object @racket[#'(F¹²³ A¹)]. A fresh scope is added to the identifiers
generated by the macro, in order to enforce macro hygiene. The @racket[A¹]
identifier is passed as an input to the macro, so it is left unchanged, and
@racket[A¹²³] is derived from @racket[F¹²³], via @racket[datum->syntax], and
therefore has the same scopes (@racket[F¹²³] is also a macro input, so it is
not tagged with the fresh scope). The @racket[Pairof¹] identifier, generated by
the macro, is however flagged with the fresh scope @racket[4]. The result of
the application of @racket[F] to this syntax object is:

@RACKETBLOCK[
 (code:comment "A¹   := Number")
 (code:comment "F¹²  := (Λ¹ (self¹ T¹)")
 (code:comment "            #`(Pairof¹ #,(datum->syntax¹ #'self¹ 'A¹)")
 (code:comment "                       T¹))")
 (code:comment "A¹²³ := String¹²")
 (Pairof¹⁴ A¹²³ A¹)]

The @racket[Pairof¹⁴] type is resolved to the primitive type constructor
@racket[Pairof]:

@RACKETBLOCK[
 (code:comment "A¹   := Number")
 (code:comment "F¹²  := (Λ¹ (self¹ T¹)")
 (code:comment "            #`(Pairof¹ #,(datum->syntax¹ #'self¹ 'A¹)")
 (code:comment "                       T¹))")
 (code:comment "A¹²³ := String¹²")
 (Pairof A¹²³ A¹)]

The type @racket[A¹²³] is then resolved to @racket[String¹²], which in turn is
resolved to the @racket[String] built-in type:

@RACKETBLOCK[
 (code:comment "A¹   := Number")
 (code:comment "F¹²  := (Λ¹ (self¹ T¹)")
 (code:comment "            #`(Pairof¹ #,(datum->syntax¹ #'self¹ 'A¹)")
 (code:comment "                       T¹))")
 (code:comment "A¹²³ := String¹²")
 (Pairof String A¹)]

And the type @racket[A¹] is resolved to @racket[Number]:

@RACKETBLOCK[
 (code:comment "A¹   := Number")
 (code:comment "F¹²  := (Λ¹ (self¹ T¹)")
 (code:comment "            #`(Pairof¹ #,(datum->syntax¹ #'self¹ 'A¹)")
 (code:comment "                       T¹))")
 (code:comment "A¹²³ := String¹²")
 (Pairof String Number)]

The @racket[syntax-local-value] function does not support querying the
transformer binding of identifiers outside of the lexical scope in which they
are bound. In our case, however, we need to access the transformer binding of
@racket[F¹²³] outside of the scope of the @racket[Let] binding it, and
similarly for @racket[A¹²³].

@section{The @racket[prop:type-expander] structure type property}

Type expanders are identified by the @tc[prop:type-expander]
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{structure type
 property}. Structure type properties allow the same identifier to act as a
rename transformer, a match expander and a type expander, for example. Such an
identifier would have to implement the @tc[prop:rename-transformer],
@tc[prop:match-expander] and @tc[prop:type-expander] properties, respectively.

@chunk[<prop:type-expander>
       (define-values (prop:type-expander
                       has-prop:type-expander?
                       get-prop:type-expander-value)
         (make-struct-type-property 'type-expander prop-guard))]

The value of the @tc[prop:type-expander] property should either be a
transformer procedure which will be called when expanding the type, or the
index of a field containing such a procedure.

@chunk[<prop-guard>
       (define (prop-guard val struct-type-info-list)
         (cond <prop-guard-field-index>
               <prop-guard-procedure>
               <prop-guard-else-error>))]

If the value is a field index, it should be within bounds. The
@tc[make-struct-field-accessor] function performs this check, and also returns
an accessor. The accessor expects an instance of the struct, and returns the
field's value.

@chunk[<prop-guard-field-index>
       [(exact-nonnegative-integer? val)
        (let* ([make-struct-accessor (cadddr struct-type-info-list)]
               [accessor (make-struct-field-accessor make-struct-accessor val)])
          (λ (instance)
            (let ([type-expander (accessor instance)])
              <prop-guard-field-value>)))]]

The expander procedure will take one argument: the piece of syntax
corresponding to the use of the expander. If the property's value is a
procedure, we therefore check that its arity includes 1.

@chunk[<prop-guard-field-value>
       (if (and (procedure? type-expander)
                (arity-includes? (procedure-arity type-expander) 1))
           type-expander
           (raise-argument-error 'prop:type-expander-guard
                                 (~a "the value of the " val "-th field should"
                                     " be a procedure whose arity includes 1")
                                 type-expander))]

In the first case, when the property value is a field index, we return an
accessor function. The accessor function expects a struct instance, performs
some checks and returns the actual type expander procedure.

When the property's value is directly a type expander procedure, we follow the
same convention. We therefore return a function which, given a struct
instance, returns the type expander procedure (ignoring the @racket[_]
argument).

@chunk[<prop-guard-procedure>
       [(procedure? val)
        (if (arity-includes? (procedure-arity val) 1)
            (λ (_) val)
            (raise-argument-error 'prop:type-expander-guard
                                  "a procedure whose arity includes 1"
                                  val))]]

When the value of the @racket[prop:type-expander] property is neither a
positive field index nor a procedure, an error is raised:

@chunk[<prop-guard-else-error>
       [else
        (raise-argument-error
         'prop:type-expander-guard
         (~a "a procedure whose arity includes 1, or an exact "
             "non-negative integer designating a field index within "
             "the structure that should contain a procedure whose "
             "arity includes 1.")
         val)]]

@subsection{The @racket[type-expander] struct}

We make a simple struct that implements @tc[prop:type-expander] and nothing
else. It has a single field, @racket[expander-proc], which contains the type
expander transformer procedure.

@chunk[<type-expander-struct>
       (struct type-expander (expander-proc) #:transparent
         #:extra-constructor-name make-type-expander
         #:property prop:type-expander (struct-field-index expander-proc))]

@section{Associating type expanders to identifiers}

@subsection{The @racket[type-expander] syntax class}

The @tc[type-expander] syntax class recognises identifiers
which are bound to type expanders. These fall into three cases:
@itemlist[
 @item{The identifier's @racket[syntax-local-value] is an instance of a struct
  implementing @racket[prop:type-expander]}
 @item{The identifier has been bound by a type-level local binding form like
  @racket[Let] or @racket[∀], and therefore are registered in the
  @racket[(tl-redirections)] binding table.}
 @item{The identifier has been patched via @racket[patch-type-expander], i.e.
  a type expander has been globally attached to an existing identifier, in which
  case the type expander is stored within the @racket[patched] free identifier
  table.}]

@chunk[<expand-type-syntax-classes>
       (define-syntax-class type-expander
         (pattern local-expander:id
                  #:when (let ([b (binding-table-find-best (tl-redirections)
                                                           #'local-expander
                                                           #f)])
                           (and b (has-prop:type-expander? b)))
                  #:with code #'local-expander)
         (pattern (~var expander
                        (static has-prop:type-expander? "a type expander"))
                  #:when (not (binding-table-find-best (tl-redirections)
                                                       #'expander
                                                       #f))
                  #:with code #'expander)
         (pattern patched-expander:id
                  #:when (let ([p (free-id-table-ref patched
                                                     #'patched-expander
                                                     #f)])
                           (and p (has-prop:type-expander? p)))
                  #:when (not (binding-table-find-best (tl-redirections)
                                                       #'expander
                                                       #f))
                  #:with code #'patched-expander))]

We also define a syntax class which matches types. Since types can bear many
complex cases, and can call type expanders which may accept arbitrary syntax,
we simply define the @tc[type] syntax class as @tc[expr]. Invalid syntax will
be eventually caught while expanding the type, and doing a thorough check
before any processing would only make the type expander slower, with little
actual benefits. The @tc[type] syntax class is however used in syntax patterns
as a form of documentation, to clarify the distinction between types and
run-time or compile-time expressions.

@CHUNK[<type-syntax-class>
       (define-syntax-class type
         (pattern :expr))]

@chunk[<type-contract>
       (define stx-type/c syntax?)]

Finally, we define a convenience syntax class which expands the matched type:

@chunk[<type-expand-syntax-class>
       (define-syntax-class type-expand!
         #:attributes (expanded)
         (pattern t:expr
                  #:with expanded (expand-type #'t #f)))]

@subsection{Calling type expanders}

The @tc[apply-type-expander] function applies the syntax expander transformer
function associated to @tc[type-expander-id]. It passes @tc[stx] as the single
argument to the transformer function. Usually, @tc[stx] will be the syntax
used to call the type expander, like @tc[#'(te arg ...)] or just @tc[#'te] if
the type expander is not in the first position of a form.

The identifier @tc[type-expander-id] should be bound to a type expander, in
one of the three possible ways described above.

@chunk[<apply-type-expander>
       (define/contract (apply-type-expander type-expander-id stx)
         (-> identifier? syntax? syntax?)
         (let ([val (or (binding-table-find-best (tl-redirections)
                                                 type-expander-id
                                                 #f)
                        (let ([slv (syntax-local-value type-expander-id
                                                       (λ () #f))])
                          (and (has-prop:type-expander? slv) slv))
                        (free-id-table-ref patched type-expander-id #f))]
               [ctxx (make-syntax-introducer)])
           <apply-type-expander-checks>
           (ctxx (((get-prop:type-expander-value val) val) (ctxx stx)))))]

The @racket[apply-type-expander] function checks that its
@racket[type-expander-id] argument is indeed a type expander before attempting
to apply it:

@chunk[<apply-type-expander-checks>
       (unless val
         (raise-syntax-error 'apply-type-expander
                             (format "Can't apply ~a, it is not a type expander"
                                     type-expander-id)
                             stx
                             type-expander-id))]

@subsection{Associating type expanders to already existing identifiers}

As explained above, existing identifiers which are provided by other libraries
can be ``patched'' so that they behave like type expanders, using a global
table associating existing identifiers to the corresponding expander code:

@chunk[<patched>
       (define patched (make-free-id-table))]

@CHUNK[<patch>
       (define-syntax patch-type-expander
         (syntax-parser
           [(_ id:id expander-expr:expr)
            #`(begin
                (begin-for-syntax
                  (free-id-table-set! patched
                                      #'id
                                      (type-expander #,(syntax/loc this-syntax
                                                         expander-expr)))))]))]

@subsection{Defining new type expanders}

The @tc[define-type-expander] macro binds @tc[_name] to a
type expander which uses @tc[(λ (_arg) . _body)] as the
transformer procedure. To achieve this, we create a
transformer binding (with @tc[define-syntax]), from 
@tc[_name] to an instance of the @tc[type-expander]
structure.

@CHUNK[<define-type-expander>
       (define-syntax define-type-expander
         (syntax-parser
           [(_ (name:id arg:id) . body)
            #`(define-syntax name
                (type-expander #,(syntax/loc this-syntax (λ (arg) . body))))]
           [(_ name:id fn:expr)
            #`(define-syntax name
                (type-expander #,(syntax/loc this-syntax fn)))]))]

@subsection[#:tag "shadow"]{Locally binding type expanders}

Some features of the type expander need to locally bind new type expanders:

@itemlist[
 @item{The @racket[(Let ([_id _expr] …) . _body)] special form binds the
  identifiers @racket[_id …] to the type expanders @racket[_expr …] in its
  @racket[_body].}
 @item{When expanding the body of a @racket[(∀ (Tᵢ …) body)] form, the
  @racket[Tᵢ] bound by the @racket[∀] may shadow some type expanders with the
  same name. If the @racket[∀] form is directly applied to arguments, each
  @racket[Tᵢ] is instead bound to the corresponding argument.}
 @item{When expanding the body of a @racket[(Rec T body)] form, the @racket[T]
  bound by the @racket[Rec] may shadow a type expander with the same name.}]

We use @racket[with-bindings] (defined in another file) to achieve this. The
code

@racketblock[(with-bindings [_bound-ids _transformer-values]
                            _rebind-stx
               _transformer-body)]

evaluates @racket[_transformer-body] in the transformer environment. It
creates a fresh scope, which it applies to the @racket[_bound-ids] and the
@racket[_rebind-stx]. It associates each modified @racket[_bound-id] with the
corresponding @racket[_transformer-value] in the @racket[(tl-redirections)]
binding table. The @racket[with-bindings] form does not mutate the syntax
objects, instead it shadows the syntax pattern variables mentioned in
@racket[_bound-ids] and @racket[_rebind-stx] with versions pointing to the
same syntax objects, but with the fresh scope flipped on them.

The code

@racketblock[(with-rec-bindings [_bound-ids _generate-transformer-values _rhs]
                                _rebind-stx
               _transformer-body)]

works in the same way, but it also flips the fresh scope on each element of
@racket[_rhs]. The @racket[_generate-transformer-values] is expected to be a
transformer expression which, given an element of @racket[_rhs] with the
flipped scope, produces the transformer value to bind to the corresponding
@racket[_bound-id].


The implementation of @racket[with-bindings] unfortunately does not play well
with @racket[syntax-local-value], so the binding table has to be queried
directly instead of using @racket[syntax-local-value]. To our knowledge, the
only ways to make new bindings recognised by @racket[syntax-local-value] are:
@itemlist[
 @item{To expand to a @racket[define-syntax] form, followed with a macro
  performing the remaining work}
 @item{Equivalently, to expand to a @racket[let-syntax] form, whose body is a
  macro performing the remaining work}
 @item{To call @racket[local-expand] with an internal definition context which
  contains the desired bindings}
 @item{To explicitly call @racket[syntax-local-value] with an internal
  definition context argument}]

It is not practical in our case to use the first solution involving
@racket[define-syntax], as the type expander may be called while expanding an
expression (e.g. @racket[ann]). The next two solutions assume that
@racket[syntax-local-value] will be called in a well-scoped fashion (in the
sense of the official expander): in the second solution,
@racket[syntax-local-value] must be called by expansion-time code located
within the scope of the @racket[let-syntax] form, and in the third solution,
@racket[syntax-local-value] must be called within the dynamic extent of
@racket[local-expand]. The last solution works, but requires that the user
explicitly passes the appropriate internal definition context.

The second and third solutions cannot be applied in our case, because type
expanders can be expanded outside of the scope in which they were defined and
used, as explained the
@secref["Interactions_between_type_expanders_and_scopes"] section.

The current version of the type expander does not support a reliable
alternative to @racket[syntax-local-value] which takes into account local
binding forms for types (@racket[Let], @racket[∀] and @racket[Rec]), but one
could be implemented, either by using some tricks to make the first solution
work, or by providing an equivalent to @racket[syntax-local-value] which
consults the @racket[(tl-redirections)] binding table.

@section{Expanding types}

The @tc[expand-type] function fully expands the type 
@tc[stx]. As explained in 
@secref["shadow"], shadowing would be better handled using
scopes. The @tc[expand-type] function starts by defining
some syntax classes, then parses @tc[stx], which can fall in
many different cases.

@CHUNK[<expand-type>
       (define (expand-type stx [applicable? #f])
         (start-tl-redirections
          <expand-type-syntax-classes>
          (define (expand-type-process stx first-pass?)
            <expand-type-debug-before>
            ((λ (result) <expand-type-debug-after>)
             (parameterize (<expand-type-debug-indent>)
               <expand-type-debug-rules>
               (syntax-parse stx
                 <expand-type-case-:>
                 <expand-type-case-expander>
                 <expand-type-case-∀-later>
                 <expand-type-case-Λ-later>
                 <expand-type-case-app-expander>
                 <expand-type-case-∀-through>
                 <expand-type-case-Rec>
                 <expand-type-case-app-Λ>
                 <expand-type-case-just-Λ/not-applicable>
                 <expand-type-case-∀-app>
                 <expand-type-case-Let>
                 <expand-type-case-Letrec>
                 <expand-type-case-noexpand>

                 (code:comment "Must be after other special application cases")
                 <expand-type-case-app-other>
                 <expand-type-case-app-fallback>
                 <expand-type-case-fallback-T>))))
          (expand-type-process stx #t)))]

@subsection{Cases handled by @racket[expand-type]}

The cases described below which expand a use of a type expander re-expand the
result, by calling @tc[expand-type] once more. This process is repeated until
no more expansion can be performed. This allows type expanders to produce
calls to other type expanders, exactly like macros can produce calls to other
macros.

We distinguish the expansion of types which will appear as the first element
of their parent form from types which will appear in other places. When the
@racket[applicable?] argument to @racket[expand-type] is @racket[#true], it
indicates that the current type, once expanded, will occur as the first
element of its enclosing form. If the expanded type is the name of a type
expander, or a @racket[∀] or @racket[Λ] form, it will be directly applied to
the given arguments by the type expander. When @racket[applicable?] is
@racket[#false], it indicates that the current type, once expanded, will
@emph{not} appear as the first element of its enclosing form (it will appear
in another position, or it is at the top of the syntax tree representing the
type).

When @racket[applicable?] is @racket[#true], if the type is the name of a type
expander, or a @racket[∀] or @racket[Λ] form, it is not expanded immediately.
Instead, the outer form will expand it with the arguments. Otherwise, these
forms are expanded without arguments, like
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macros} would be.

@subsection{Applying type expanders}

When a type expander is found in a non-applicable position, it is called,
passing the identifier itself to the expander. An
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro} would be
called in the same way.

@CHUNK[<expand-type-case-expander>
       [expander:type-expander
        #:when (not applicable?)
        (rule id-expander/not-applicable
          (let ([ctxx (make-syntax-introducer)])
            (expand-type (ctxx (apply-type-expander #'expander.code
                                                    (ctxx #'expander)))
                         applicable?)))]]

When a type expander is found in an applicable position, it is returned
without modification, so that the containing application form may expand it
with arguments. When the expander @racket[e] appears as @racket[(e . args)],
it is applicable. It is also applicable when it appears as
@racket[((Let (bindings…) e) . args)], for example, because @racket[Let]
propagates its @racket[applicable?] status.

@CHUNK[<expand-type-case-expander>
       [expander:type-expander
        #:when applicable?
        (rule id-expander/applicable
          #'expander)]]

When a form contains a type expander in its first element, the type expander
is called. The result is re-expanded, so that a type expander can expand to a
use of another type expander.

@CHUNK[<expand-type-case-app-expander>
       [(~and expander-call-stx (expander:type-expander . _))
        (rule app-expander
          (let ([ctxx (make-syntax-introducer)])
            (expand-type (ctxx (apply-type-expander #'expander.code
                                                    (ctxx #'expander-call-stx)))
                         applicable?)))]]

When a form of the shape @racket[(_f . _args)] is encountered, and the
@racket[_f] element is not a type expander, the @racket[_f] form is expanded,
and the whole form (with @racket[_f] replaced by its expansion) is expanded a
second time. The @racket[applicable?] parameter is set to @racket[#true] while
expanding @racket[_f], so that if @racket[_f] produces a type expander (e.g.
@racket[_f] has the shape @racket[(Let (…) _some-type-expander)]), the type
expander can be applied to the @racket[_args] arguments.

@CHUNK[<expand-type-case-app-other>
       [(~and whole (f . args))
        #:when first-pass?
        (rule app-other
          (expand-type-process
           (datum->syntax #'whole
                          (cons (expand-type #'f #true) #'args)
                          #'whole
                          #'whole)
           #f))]]

@subsubsection{Polymorphic types with @racket[∀]}

When the @tc[∀] or @tc[All] special forms from @racketmodname[typed/racket]
are used, the bound type variables may shadow some type expanders. The type
expanders used in the body @tc[T] which have the same identifier as a bound
variable will be affected by this (they will not act as a type-expander
anymore). The body of the @tc[∀] or @tc[All] form is expanded with the
modified environment. The result is wrapped again with
@tc[(∀ (TVar …) expanded-T)], in order to conserve the behaviour from
@racketmodname[typed/racket]'s @tc[∀].

@CHUNK[<expand-type-case-∀-through>
       [({~and ∀ {~literal ∀}} (tvar:id …) T:type)
        #:when (not applicable?)
        (rule just-∀/not-applicable
          (with-syntax ([(tvar-vars-only …) (remove-ddd #'(tvar …))])
            (with-bindings [(tvar-vars-only …) (stx-map <shadowed>
                                                        #'(tvar-vars-only …))]
                           (T tvar …)
              #`(∀ (tvar …)
                   #,(expand-type #'T #f)))))]]

Where @racket[<shadowed>] is used to bind the type variables @racket[tvarᵢ] to
@racket[(No-Expand tvarᵢ)], so that their occurrences are left intact by the
type expander:

@CHUNK[<shadowed>
       (λ (__τ)
         (make-type-expander
          (λ (stx)
            (syntax-case stx ()
              [self (identifier? #'self) #'(No-Expand self)]
              [(self . args) #'((No-Expand self) . args)]))))]

When a @racket[∀] polymorphic type is found in an applicable position, it is
returned without modification, so that the containing application form may
expand it, binding the type parameters to their effective arguments.

@CHUNK[<expand-type-case-∀-later>
       [(~and whole ({~literal ∀} (tvar:id …) T:type))
        #:when applicable?
        (rule just-∀/applicable
          #'whole)]]

When a @racket[∀] polymorphic type is immediately applied to arguments, the
type expander attempts to bind the type parameters to the effective arguments.
It currently lacks any support for types under ellipses, and therefore that
case is currently handled by the @racket[<expand-type-case-app-fallback>] case
described later.

@chunk[<expand-type-case-∀-app>
       [(({~literal ∀} ({~and tvar:id {~not {~literal …}}} …) τ) arg …)
        (unless (= (length (syntax->list #'(tvar …)))
                   (length (syntax->list #'(arg …))))
          <app-args-error>)
        (rule app-∀
          (with-bindings [(tvar …) (stx-map (λ (a) (make-type-expander (λ (_) a)))
                                                #'(arg …))]
                             τ
                (expand-type #'τ applicable?)))]]

If the given number of arguments does not match the expected number of
arguments, an error is raised immediately:

@chunk[<app-args-error>
       (raise-syntax-error
        'type-expander
        (format (string-append "Wrong number of arguments to "
                               "polymorphic type: ~a\n"
                               "  expected: ~a\n"
                               "  given: ~a"
                               "  arguments were...:\n")
                (syntax->datum #'f)
                (length (syntax->list #'(tvar …)))
                (length (syntax->list #'(arg …)))
                (string-join
                 (stx-map (λ (a)
                            (format "~a" (syntax->datum a)))
                          #'(arg …))
                 "\n"))
        #'whole
        #'∀
        (syntax->list #'(arg …)))]

@subsubsection{Recursive types with @racket[Rec]}

Similarly, the @tc[Rec] special form will cause the bound
variable @tc[R] to shadow type expanders with the same name,
within the extent of the body @tc[T]. The result is wrapped
again with @tc[(Rec R expanded-T)], in order to conserve the
behaviour from @racketmodname[typed/racket]'s @tc[Rec].

@CHUNK[<expand-type-case-Rec>
       [((~literal Rec) R:id T:type)
        (rule Rec
          #`(Rec R #,(with-bindings [R (<shadowed> #'R)]
                                        T
                           (expand-type #'T #f))))]]

@subsubsection{Local bindings with @racket[Let] and @racket[Letrec]}

The @tc[Let] special form binds the given identifiers to the corresponding
type expanders. We use @racket[with-bindings], as explained above in
@secref["shadow" #:doc '(lib "type-expander/type-expander.hl.rkt")], to bind
the @racket[Vᵢ …] identifiers to their corresponding @racket[Eᵢ] while
expanding @racket[T].

@CHUNK[<expand-type-case-Let>
       [((~commit (~literal Let)) ([Vᵢ:id Eᵢ] …) T:type)
        (rule Let
          (with-bindings [(Vᵢ …)
                          (stx-map (λ (Eᵢ)
                                     (make-type-expander
                                      (λ (stx)
                                        (syntax-case stx ()
                                          [self (identifier? #'self) Eᵢ]
                                          [(self . argz) #`(#,Eᵢ . argz)]))))
                                   #'(Eᵢ …))]
                         T
            (expand-type #'T applicable?)))]]

The @tc[Letrec] special form behaves in a similar way, but uses
@racket[with-rec-bindings], so that the right-hand-side expressions
@racket[Eᵢ] appear to be within the scope of all the @racket[Vᵢ] bindings.

@CHUNK[<expand-type-case-Letrec>
       [((~commit (~literal Letrec)) ([Vᵢ:id Eᵢ] …) T:type)
        (rule Letrec
          (with-rec-bindings [(Vᵢ …)
                              (λ (Eᵢ)
                                (make-type-expander
                                 (λ (stx)
                                   (syntax-case stx ()
                                     [self (identifier? #'self) Eᵢ]
                                     [(self . args444) #`(#,Eᵢ . args444)]))))
                              Eᵢ]
                             T
            (expand-type #'T applicable?)))]]

@subsubsection{Anonymous types with @racket[Λ]}

When an anonymous type expander appears as the first element of its enclosing
form, it is applied to the given arguments. We use the
@racket[trampoline-eval] function defined in another file, which evaluates the
given quoted transformer expression, while limiting the issues related to
scopes. The ``official'' @racket[eval] function from
@racketmodname[racket/base] removes one of the module scopes which are
normally present on the expression to evaluate. In our case, we are evaluating
an anonymous type expander, i.e. a transformer function. When using
@racket[eval], identifiers generated by the transformer function may not have
the expected bindings. The alternative @racket[trampoline-eval] seems to solve
this problem.

The @racket[auto-syntax-case] form is used, so that an anonymous type expander
@racket[(Λ (_ a b) …)] can either use @racket[a] and @racket[b] as pattern
variables in quoted syntax objects, or as regular values (i.e
@racket[syntax->datum] is automatically applied on the syntax pattern
variables when they are used outside of syntax templates, instead of throwing
an error).

@chunk[<eval-anonymous-expander-code>
       (trampoline-eval
        #'(λ (stx)
            (define ctxx (make-syntax-introducer))
            (ctxx (auto-syntax-case (ctxx (stx-cdr stx)) ()
                    [formals (let () . body)]))))]

This case works by locally binding a fresh identifier @racket[tmp] to a type
expander, and then applying that type expander. It would also be possible to
immediately invoke the type expander function.

@chunk[<expand-type-case-app-Λ>
       [{~and whole (({~literal Λ} formals . body) . __args)}
        ;; TODO: use the same code as for the not-applicable case, to avoid ≠
        (rule app-Λ
          (with-syntax* ([tmp (gensym '#%Λ-app-)]
                         [call-stx #'(tmp . whole)])
            (with-bindings [tmp (make-type-expander
                                 <eval-anonymous-expander-code>)]
                           call-stx
              (expand-type #'call-stx applicable?))))]]

When a @racket[Λ] anonymous type expander appears on its own, in a
non-applicable position, it is expanded like an
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro} would
be.

This case is implemented like the @racket[<expand-type-case-app-Λ>] case, i.e.
by locally binding a fresh identifier @racket[tmp] to a type expander, and
then applying that type expander. The difference is that in the
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro} case,
the syntax object given as an argument to the type expander contains only the
generated @racket[tmp] identifier. This allows the type expander to easily
recognise the @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier
 macro} case, where the whole syntax form is an identifier, from regular
applications, where the whole syntax form is a syntax pair. The whole original
syntax is attached @racket[cons]ed onto a syntax property named
@racket['original-Λ-syntax], in (unlikely) case the type expander needs to
access the original @racket[Λ] syntax used to call it (this is an experimental
feature, and may change without notice in later versions).

@CHUNK[<expand-type-case-just-Λ/not-applicable>
       [{~and whole ({~literal Λ} formals . body)}
        #:when (not applicable?)
        (rule just-Λ/not-applicable
          (with-syntax* ([tmp (syntax-property
                               (datum->syntax #'whole
                                              (gensym '#%Λ-id-macro-)
                                              #'whole
                                              #'whole)
                               'original-Λ-syntax
                               (cons #'whole
                                     (or (syntax-property #'whole
                                                          'original-Λ-syntax)
                                         null)))]
                         [call-stx #'(tmp . tmp)])
                (with-bindings [tmp (make-type-expander
                                     <eval-anonymous-expander-code>)]
                               call-stx
                  ;; applicable? should be #f here, otherwise it would have been
                  ;; caught by other cases.
                  (expand-type #'call-stx applicable?))))]]

When a @racket[Λ] anonymous type expander appears on its own, in an applicable
position, it is returned without modification, so that the containing
application form may expand it with arguments (instead of expanding it like an
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro} would
be).

@CHUNK[<expand-type-case-Λ-later>
       [(~and whole ({~literal Λ} formals . body))
        #:when applicable?
        (rule just-Λ/applicable
          #'whole)]]

@subsubsection{Preventing the expansion of types with @racket[No-Expand]}

The @racket[No-Expand] special form prevents the type expander from
re-expanding the result. This is useful for example for the implementation of
the fancy @racket[quote] expander, which relies on the built-in @racket[quote]
expander. It is also used to implement shadowing: type variables bound by
@racket[∀] in non-applicable positions and type variables bound by
@racket[Rec] are re-bound to type expanders returning
@racket[(No-Expand original-tvar)].

@CHUNK[<expand-type-case-noexpand>
       [((~literal No-Expand) T)
        (rule just-No-Expand
          #'T)]
       [(((~literal No-Expand) T) arg ...)
        (rule app-No-Expand
          #`(T #,@(stx-map (λ (τ) (expand-type τ #f)) #'(arg ...))))]]

@subsubsection{The overloaded @racket[:] identifier}

This case handles the colon identifiers @tc[:] (overloaded by this
library) and @|orig::| (provided by @racketmodname[typed/racket]). Wherever
the new overloaded @tc[:] identifier appears in a type, we want to convert it
back to the original @orig:: from @racketmodname[typed/racket]. The goal is
that a type of the form @racket[(→ Any Boolean : Integer)], using the new
@tc[:], will get translated to @orig:→AnyBoolean:Integer, using the old
@orig:: so that it gets properly interpreted by @racketmodname[typed/racket]'s
parser.

@chunk[<expand-type-case-:>
       [(~and c (~literal new-:))
        (rule (datum->syntax #'here ': #'c #'c)
          ':)]]

@subsubsection{Last resort cases: leaving the type unchanged}

If the type expression to expand was not matched by any of the above cases,
then it can still be an application of a polymorphic type @tc[T]. The
arguments @tc[TArg …] can contain uses of type expanders. We therefore expand
each separately, and combine the results.

@CHUNK[<expand-type-case-app-fallback>
       [{~and whole (T TArg …)}
        (rule app-fallback
          (quasisyntax/loc #'whole
            (T #,@(stx-map (λ (a) (expand-type a #f)) #'(TArg ...)))))]]

As a last resort, we consider that the type @tc[T] (which
would most likely be an identifier) is either a built-in
type provided by @tc[typed/racket], or a user-declared type
introduced by @tc[define-type]. In both cases, we just leave
the type as-is.

@CHUNK[<expand-type-case-fallback-T>
       [T
        (rule just-fallback
          #'T)]]

@subsection{Debugging type expanders}

In order to facilitate writing type expanders, it is possible to print the
inputs, steps and outputs of the expander using
@racket[(debug-type-expander #t)], which sets the value of
@racket[debug-type-expander?]. This can then be undone using
@racket[(debug-type-expander #f)].

@chunk[<expand-type-debug-outer>
       (define debug-type-expander? (box #f))]

@chunk[<debug-type-expander>
       (define-syntax (debug-type-expander stx)
         (syntax-case stx ()
           [(_ #t) (set-box! debug-type-expander? #t) #'(void)]
           [(_ #f) (set-box! debug-type-expander? #f) #'(void)]))]

For better readability, each level of recursion indents the debugging
information:

@chunk[<expand-type-debug-outer>
       (define indent (make-parameter 0))]

@chunk[<expand-type-debug-indent>
       [indent (+ (indent) 3)]]

Before expanding a term, it is printed:

@chunk[<expand-type-debug-before>
       (when (unbox debug-type-expander?)
         (printf "~a~a ~a"
                 (make-string (indent) #\ )
                 applicable?
                 (+scopes stx)))]

Once the term has been expanded, the original term and the expanded term are
printed:

@chunk[<expand-type-debug-after>
       (when (unbox debug-type-expander?)
         (printf "~a~a ~a\n~a=> ~a (case: ~a)\n"
                 (make-string (indent) #\ )
                 applicable?
                 (+scopes stx)
                 (make-string (indent) #\ )
                 (+scopes (car result))
                 (cdr result))
         (when (= (indent) 0)
           (print-full-scopes)))
       (car result)]

Finally, each rule for the type expander is wrapped with the @racket[rule]
macro, which prints the name of the rule, and returns a pair containing the
result and the rule's name, so that the debugging information indicates the
rule applied at each step.

@chunk[<expand-type-debug-rules>
       (define-syntax-rule (rule name e)
         (begin (when (unbox debug-type-expander?)
                  (printf "(case:~a)\n"
                          'name))
                (cons e 'name)))]

@section{Overloading @racket[typed/racket] forms}

Throughout this section, we provide alternative definitions of the
@tc[typed/racket] forms @tc[:], @tc[lambda], @tc[define], @tc[struct], @tc[ann],
@tc[inst]… . We write these definitions with @tc[syntax-parse], using the syntax
classes defined in section @secref{type-expander|syntax-classes}.

Most of the time, we will use the experimental @tc[template] macro from
@tc[syntax/parse/experimental/template] which allows more concise code than the
usual @code{#'()} and @code{#`()}.

@subsection[#:tag "type-expander|syntax-classes"]{syntax classes}

The syntax classes from 
@tc[typed-racket/base-env/annotate-classes] match against
the @orig:: literal. Since we provide a new definition for
it, these syntax classes do not match code using our
definition of @tc[:]. We therefore cannot use the original
implementations of @tc[curried-formals] and 
@tc[lambda-formals], and instead have to roll out our own
versions.

We take that as an opportunity to expand the types directly from the syntax
classes using @tc[#:with], instead of doing that inside the macros that use
them.

The @tc[colon] syntax class records the identifier it matches as a "disappeared
use", which means that DrRacket will draw an arrow from the library importing it
(either @racketmodname[typed/racket] or @racketmodname[type-expander]) to the
identifier. Unfortunately, this effect is not (yet) undone by 
@racketmodname[syntax/parse]'s backtracking. See
@url{https://groups.google.com/forum/#!topic/racket-users/Nc1klmsj9ag} for more
details about this.

@chunk[<remove-ddd>
       (define (remove-ddd stx)
         (remove #'(... ...) (syntax->list stx) free-identifier=?))]

@CHUNK[<syntax-classes>
       (define-syntax-class colon
         #:attributes ()
         (pattern (~and {~or {~literal new-:} {~literal :}}
                        C
                        {~do (record-disappeared-uses (list #'C))})))

       (define-splicing-syntax-class new-maybe-kw-type-vars
         #:attributes ([vars 1] maybe)
         (pattern kw+vars:lambda-type-vars
                  #:with (vars …) (remove-ddd #'kw+vars.type-vars)
                  #:with maybe #'kw+vars)
         (pattern (~seq)
                  #:with (vars …) #'()
                  #:attr maybe #f))
       
       (define-splicing-syntax-class new-maybe-type-vars
         #:attributes ([vars 1] maybe)
         (pattern v:type-variables
                  #:with (vars …) (remove-ddd #'v)
                  #:with maybe #'v)
         (pattern (~seq)
                  #:with (vars …) #'()
                  #:attr maybe #f))
       
       (define-splicing-syntax-class new-kw-formal
         #:attributes ([expanded 1])
         (pattern (~seq kw:keyword id:id)
                  #:with (expanded ...) #'(kw id))
         (pattern (~seq kw:keyword [id:id
                                    (~optional (~seq :colon type:type-expand!))
                                    (~optional default:expr)])
                  #:with (expanded ...)
                  (template (kw [id (?@ : type.expanded)
                                 (?? default)]))))
       
       (define-splicing-syntax-class new-mand-formal
         #:attributes ([expanded 1])
         (pattern id:id
                  #:with (expanded ...) #'(id))
         (pattern [id:id :colon type:type-expand!]
                  #:with (expanded ...)
                  (template ([id : type.expanded])))
         (pattern kw:new-kw-formal
                  #:with (expanded ...) #'(kw.expanded ...)))
       
       (define-splicing-syntax-class new-opt-formal
         #:attributes ([expanded 1])
         (pattern [id:id
                   (~optional (~seq :colon type:type-expand!))
                   default:expr]
                  #:with (expanded ...)
                  (template ([id (?? (?@ : type.expanded))
                              default])))
         (pattern kw:new-kw-formal
                  #:with (expanded ...) #'(kw.expanded ...)))
       
       (define-syntax-class new-rest-arg
         #:attributes ([expanded 0])
         (pattern rest:id
                  #:with expanded #'rest)
         (pattern (rest:id
                   :colon type:type-expand!
                   (~or (~and x* (~describe "*" (~or (~literal *)
                                                     (~literal ...*))))
                        (~seq (~literal ...) bound:type-expand!)))
                  #:with expanded
                  (template (rest : type.expanded
                                  (?? x*
                                      (?@ (... ...) bound.expanded))))))
       
       (define-syntax-class new-lambda-formals
         (pattern (~or (mand:new-mand-formal ...
                        opt:new-opt-formal ...
                        . rest:new-rest-arg)
                       (mand:new-mand-formal ...
                        opt:new-opt-formal ...))
                  ;; TODO: once template supports ?? in tail position, use it.
                  #:with expanded #`(mand.expanded ...
                                     ...
                                     opt.expanded ...
                                     ...
                                     . #,(if (attribute rest)
                                             #'rest.expanded
                                             #'()))))
       
       (define-syntax-class (new-curried-formals def-id)
         (pattern (f:id . args:new-lambda-formals)
                  #:with expanded #`(#,def-id . args.expanded))
         (pattern ((~var lhs (new-curried-formals def-id))
                   . args:new-lambda-formals)
                  #:with expanded #'(lhs.expanded . args.expanded)))

       (define-syntax-class new-curried-formals-id
         (pattern (id:id . _))
         (pattern (lhs:new-curried-formals-id . _)
                  #:with id #'lhs.id))
       
       (define-splicing-syntax-class new-optionally-annotated-name
         (pattern (~seq name:id (~optional (~seq :colon type:type-expand!)))
                  #:with expanded
                  (template (name
                             (?? (?@ : type.expanded))))))
       
       (define-syntax-class new-name-or-parenthesised-annotated-name
         (pattern name:id
                  #:with expanded #'name)
         (pattern [id:id :colon type:type-expand!]
                  #:with expanded
                  (template [id : type.expanded])))]

@subsection{Overview of the overloaded primitives}

The following sections merely define overloads for the 
@racketmodname[typed/racket] primitives. The process is
similar each time: a new primitive is defined, e.g. 
@tc[new-:] for @|orig::|. The new primitive calls the old one,
after having expanded (using @tc[expand-type]) all parts of
the syntax which contain types. Aside from heavy usage of 
@tc[syntax-parse], there is not much to say concerning these
definitions.

@subsection{@racket[:]}

@CHUNK[<:>
       (set-:-impl! (syntax-parser
                      [(_ x:id t:expr)
                       #`(: x #,(expand-type #'t #f))]))]

@subsection{@racket[define-type]}

@chunk[<define-type>
       (define-syntax new-define-type
         (syntax-parser
           [(_ (~or name:id (name:id maybe-tvar …)) . whole-rest)
            #:with (tvar …) (if (attribute maybe-tvar) #'(maybe-tvar …) #'())
            #:with (tvar-not-ooo …) (filter (λ (tv) (not (free-identifier=? tv #'(… …))))
                                            (syntax->list #'(tvar …)))
            (start-tl-redirections
             (with-bindings [(tvar-not-ooo …) (stx-map <shadowed>
                                                       #'(tvar-not-ooo …))]
                            whole-rest
               (syntax-parse #'whole-rest
                 [(type:type-expand! . rest)
                  (template
                   (define-type (?? (name tvar …) name)
                     type.expanded
                     . rest))])))]))]

@subsection{@racket[define]}

@chunk[<define>
       (define-syntax new-define
         (f-start-tl-redirections
          (syntax-parser
            [(_ {~and (~seq _:new-maybe-kw-type-vars
                            (~or v:id
                                 formals-id:new-curried-formals-id)
                            _ …)
                     (~with-tvars (tvars new-maybe-kw-type-vars)
                                  (~or _:id
                                       (~var formals (new-curried-formals
                                                      #'formals-id.id)))
                                  (~optional (~seq :colon type:type-expand!))
                                  e ...)})
             (template
              (define (?? (?@ . tvars.maybe)) (?? v formals.expanded)
                (?? (?@ : type.expanded))
                e ...))])))]

@subsection{@racket[lambda]}

@CHUNK[<lambda>
       (define-syntax new-lambda
         (f-start-tl-redirections
          (syntax-parser
            [(_ {~with-tvars (tvars new-maybe-kw-type-vars)
                     args:new-lambda-formals
                     (~optional (~seq :colon ret-type:type-expand!))
                     e …})
             (template (lambda (?? (?@ . tvars.maybe)) args.expanded
                         (?? (?@ : ret-type.expanded))
                         e ...))])))]

@subsection{@racket[case-lambda]}

@CHUNK[<case-lambda>
       (define-syntax new-case-lambda
         (f-start-tl-redirections
          (syntax-parser
            [(_ {~with-tvars (tvars new-maybe-kw-type-vars)
                     [args:new-lambda-formals
                      (~optional (~seq :colon ret-type:type-expand!))
                      e …]
                     …})
             (template (case-lambda
                         (?? (?@ #:∀ tvars.maybe))
                         [args.expanded
                          (?? (ann (let () e …) ret-type.expanded)
                              (?@ e …))]
                         …))])))]

@subsection{@racket[struct]}

The name must be captured outside of the @racket[~with-tvars], as
@racket[~with-tvars] introduces everything in a new lexical context.

@chunk[<struct>
       (define-syntax new-struct
         (f-start-tl-redirections
          (syntax-parser
            [(_ (~and
                 (~seq _:new-maybe-type-vars
                       (~and (~seq name+parent …)
                             (~or (~seq name:id)
                                  (~seq name:id parent:id)))
                       _ …)
                 {~with-tvars (tvars new-maybe-type-vars)
                     (~or (~seq _:id)
                          (~seq _:id _:id))
                     ([field:id :colon type:type-expand!] ...)
                     rest …}))
             (template (struct (?? tvars.maybe) name (?? parent)
                         ([field : type.expanded] ...)
                         rest …))])))]

@subsection{@racket[define-struct/exec]}

@chunk[<define-struct/exec>
       (define-syntax (new-define-struct/exec stx)
         (syntax-parse stx
           [(_ (~and name+parent (~or name:id [name:id parent:id]))
               ([field:id (~optional (~seq :colon type:type-expand!))] ...)
               [proc :colon proc-type:type-expand!])
            (template (define-struct/exec name+parent
                        ([field (?? (?@ : type.expanded))] ...)
                        [proc : proc-type.expanded]))]))]

@subsection{@racket[ann]}

@chunk[<ann>
       (define-syntax/parse (new-ann value:expr
                                     (~optional :colon) type:type-expand!)
         (template (ann value type.expanded)))]

@subsection{@racket[cast]}

@chunk[<cast>
       (define-syntax/parse (new-cast value:expr type:type-expand!)
         (template (cast value type.expanded)))]

@subsection{@racket[unsafe-cast]}

We additionally define an @racket[unsafe-cast] macro, which Typed/Racket does
not provide yet, but can easily be defined using @racket[unsafe-require/typed]
and a polymorphic function.

@chunk[<unsafe-cast>
       (module m-unsafe-cast typed/racket
         (provide unsafe-cast-function)
         (define (unsafe-cast-function [v : Any]) v))
  
       (require (only-in typed/racket/unsafe unsafe-require/typed))
       (unsafe-require/typed 'm-unsafe-cast
                             [unsafe-cast-function (∀ (A) (→ Any A))])
         
       (define-syntax-rule (unsafe-cast/no-expand v t)
         ((inst unsafe-cast-function t) v))

       (define-syntax/parse (unsafe-cast value:expr type:type-expand!)
         (template (unsafe-cast/no-expand value type.expanded)))]

@subsection{@racket[inst]}

@chunk[<inst>
       (define-syntax new-inst
         (syntax-parser
           [(_ v (~optional :colon) t:type-expand! ...
               last:type-expand! (~literal ...) b:id)
            (template (inst v
                            t.expanded ...
                            last.expanded (... ...) b))]
           [(_ v (~optional :colon) t:type-expand! ...)
            (template (inst v t.expanded ...))]))]

@subsection{@racket[row-inst]}

@chunk[<row-inst>
       (define-syntax/parse (new-inst e row:type-expand!)
         (template (row-inst e row.expanded)))]

@subsection{@racket[let]}

@chunk[<let>
       (define-syntax new-let
         (f-start-tl-redirections
          (syntax-parser
            [(_ (~optional (~seq loop:id
                                 (~optional
                                  (~seq :colon return-type:type-expand!))))
                (~with-tvars (tvars new-maybe-kw-type-vars)
                             ([name:new-optionally-annotated-name e:expr] ...)
                             rest ...))
             (template
              (let (?? (?@ loop (?? (?@ : return-type.expanded))))
                (?@ . tvars)
                ([(?@ . name.expanded) e] ...)
                rest ...))])))]

@subsection{@racket[let*]}

@chunk[<let*>
       (define-syntax/parse
           (new-let*
            ([name:new-optionally-annotated-name e:expr] ...)
            . rest)
         (template
          (let* ([(?@ . name.expanded) e] ...) . rest)))]

@subsection{@racket[let-values]}

@chunk[<let-values>
       (define-syntax/parse
           (new-let-values
            ([(name:new-name-or-parenthesised-annotated-name ...) e:expr] ...)
            . rest)
         (template
          (let-values ([(name.expanded ...) e] ...)
            . rest)))]

@subsection{@racket[make-predicate]}

@chunk[<make-predicate>
       (define-simple-macro (new-make-predicate type:type-expand!)
         (make-predicate type.expanded))]

@subsection{@racket[:type], @racket[:print-type], @racket[:query-type/args],
 @racket[:query-type/result]}

@chunk[<:type>
       (define-syntax/parse (new-:type (~optional (~and verbose #:verbose))
                                       type:type-expand!)
         (template (eval #'(#%top-interaction
                            . (:type (?? verbose) type.expanded)))))]

@chunk[<:print-type>
       (define-syntax/parse (new-:print-type e:expr)
         #'(:print-type e)
         #'(eval #'(#%top-interaction
                    . (:print-type e))))]

@chunk[<:query-type/args>
       (define-syntax/parse (new-:query-type/args f type:type-expand! …)
         #'(eval #'(#%top-interaction
                    . (:query-type/args f type.expanded …))))]

@chunk[<:query-type/result>
       (define-syntax/parse (new-:query-type/result f type:type-expand!)
         #'(eval #'(#%top-interaction
                    . (:query-type/result f type.expanded))))]

@subsection{Type expanders for the typed classes}

Not all forms are supported for now.

@chunk[<syntax-classes>
       (define-syntax-class field-decl
         (pattern id:id #:with expanded #'(field id))
         (pattern (maybe-renamed {~optional {~seq :colon type:type-expand!}}
                                 {~optional default-value-expr})
                  #:with expanded
                  (template (maybe-renamed (?? (?@ : type.expanded))
                                           (?? default-value-expr)))))]

@chunk[<syntax-classes>
       (define-syntax-class field-clause
         #:literals (field)
         (pattern (field field-decl:field-decl …)
                  #:with expanded (template (field field-decl.expanded …))))]

@chunk[<syntax-classes>
       (define-syntax-class super-new-clause
         #:literals (super-new)
         (pattern (super-new . rest)
                  #:with expanded (template (super-new . rest))))]

@;{
 @chunk[<field>
        (set-field-impl!
         (syntax-parser [clause:field-clause #'clause.expanded]))]

 @chunk[<super-new>
        (set-super-new-impl!
         (syntax-parser [clause:super-new-clause #'clause.expanded]))]}


@chunk[<syntax-classes>
       (define-syntax-class class-clause
         #:attributes (expanded)
         (pattern :field-clause)
         (pattern :super-new-clause))]

@chunk[<class>
       (define-syntax new-class
         (f-start-tl-redirections
          (syntax-parser
            [(_ superclass-expr
                {~with-tvars (tvars new-maybe-kw-type-vars)
                     clause:class-clause ...})
             (template (class superclass-expr
                         (?? (?@ . tvars.maybe))
                         clause.expanded ...))])))]

@subsection[#:tag "type-expander|other-forms"]{Other @racket[typed/racket]
 forms}

The other @tc[typed/racket] forms below do not have an alternative definition
yet.

@chunk[<other-forms>
       (define-syntax (missing-forms stx)
         (syntax-parse stx
           [(_ name ...)
            (define/with-syntax (tmp ...) (generate-temporaries #'(name ...)))
            #'(begin
                (begin
                  (define-syntax (tmp stx)
                    (raise-syntax-error
                     'name
                     (format "~a not implemented yet for type-expander" 'name)
                     stx))
                  (provide (rename-out [tmp name])))
                ...)]))
       
       (missing-forms
        (code:comment ";TODO: add all-defined-out in prims.rkt")
        (code:comment "; top-interaction.rkt")
        (code:comment ":type")
        (code:comment ":print-type")
        (code:comment ":query-type/args")
        (code:comment ":query-type/result")
        (code:comment "; case-lambda.rkt")
        (code:comment "case-lambda")
        (code:comment "case-lambda:")
        pcase-lambda:
        (code:comment "; (submod \"prims-contract.rkt\" forms)")
        require/opaque-type 
        (code:comment "require-typed-struct-legacy")
        require-typed-struct
        (code:comment "require/typed-legacy")
        require/typed
        require/typed/provide
        require-typed-struct/provide
        (code:comment "cast")
        (code:comment "make-predicate")
        define-predicate
        (code:comment "; prims.rkt")
        define-type-alias
        define-new-subtype
        define-typed-struct
        define-typed-struct/exec
        (code:comment "ann")
        (code:comment "inst")
        (code:comment ":")
        define-struct:
        define-struct
        (code:comment "struct")
        struct:
        λ:
        lambda:
        (code:comment "lambda")
        (code:comment "λ")
        (code:comment "define")
        (code:comment "let")
        (code:comment "let*")
        letrec
        (code:comment "let-values")
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
        (code:comment "define-struct/exec"))]

@section{Future work}

We have not implemented alternative type-expanding definitions for all the
@tc[typed/racket] forms, as noted in @secref{type-expander|other-forms}.

Integrating the type expander directly into typed/racket
would avoid the need to provide such definitions, and allow
using type expanders in vanilla @tc[typed/racket], instead
of having to @racket[require] this library. However, the
code wrapping the @tc[typed/racket] forms could be re-used
by other libraries that alter the way @tc[typed/racket]
works, so implementing the remaining forms could still be
useful.

Also, we would need to provide a @tc[syntax-local-type-introduce] function,
similar to the @tc[syntax-local-match-introduce] function provided by @tc[match]
for example.

@section{Conclusion}

When an identifier is @racket[require]d from another module,
it is not the same as the one visible within the defining
module. This is a problem for @tc[:], because we match
against it in our syntax classes, using @tc[(~literal :)],
but when it is written in another module, for example 
@tc[(define foo : Number 42)], it is not the same identifier
as the one used by original definition of @tc[:], and
therefore the @tc[(~literal :)] won't match. I suspect that
issue to be due to contract wrappers added by 
@tc[typed/racket].

To get around that problem, we define @tc[:] in a separate module, and
@racket[require] it in the module containing the syntax classes:

Since our @tc[new-:] macro needs to call the 
@tc[type-expander], and the other forms too, we cannot
define @tc[type-expander] in the same module as these forms,
it needs to be either in the same module as @tc[new-:], or
in a separate module. Additionally, @tc[expand-type] needs
to be required @tc[for-syntax] by the forms, but needs to be
@tc[provide]d too, so it is much easier if it is defined in
a separate module (that will be used only by macros, so it
will be written in @tc[racket], not @tc[typed/racket]).

@chunk[<module-expander>
       (module expander racket
         (require (for-template typed/racket
                                "identifiers.rkt")
                  racket
                  (only-in racket/base [... …])
                  syntax/parse
                  racket/format
                  racket/syntax
                  syntax/id-table
                  syntax/stx
                  auto-syntax-e
                  "parameterize-lexical-context.rkt"
                  debug-scopes)
         ;; TODO: move this in a separate chunk and explain it
         
         (provide prop:type-expander
                  type-expander
                  apply-type-expander
                  ;bind-type-vars
                  expand-type
                  type
                  stx-type/c
                  type-expand!
                  debug-type-expander?
                  patched
                  make-type-expander)

         <remove-ddd>
         
         <prop-guard>
         <prop:type-expander>
         <type-expander-struct>

         <patched>
         
         <apply-type-expander>
         ;<expand-quasiquote>
         <type-syntax-class>
         <type-contract>
         <expand-type-debug-outer>
         <expand-type>
         <type-expand-syntax-class>)]

We can finally define the overloaded forms, as well as the 
@tc[<define-type-expander>] form.

@chunk[<module-main>
       (module main typed/racket
         (require (only-in typed/racket/base [... …])
                  typed/racket/class
                  (for-syntax racket
                              (only-in racket/base [... …])
                              racket/syntax
                              syntax/parse
                              syntax/parse/experimental/template
                              syntax/id-table
                              "parameterize-lexical-context.rkt"
                              syntax/stx)
                  (for-meta 2 racket/base syntax/parse)
                  "utils.rkt"
                  syntax/parse/define
                  "identifiers.rkt")
         
         (require (submod ".." expander))
         (require (for-syntax (submod ".." expander)))
         (require (for-syntax typed-racket/base-env/annotate-classes))
         
         (provide prop:type-expander
                  expand-type
                  define-type-expander
                  patch-type-expander
                  Let
                  Letrec
                  Λ
                  ...*
                  No-Expand
                  unsafe-cast/no-expand
                  unsafe-cast
                  debug-type-expander
                  (rename-out [new-: :]
                              [new-define-type        define-type]
                              [new-define             define]
                              [new-lambda             lambda]
                              [new-lambda             λ]
                              [new-case-lambda        case-lambda]
                              [new-case-lambda        case-lambda:]
                              [new-struct             struct]
                              [new-define-struct/exec define-struct/exec]
                              [new-ann                ann]
                              [new-cast               cast]
                              [new-inst               inst]
                              [new-let                let]
                              [new-let*               let*]
                              [new-let-values         let-values]
                              [new-make-predicate     make-predicate]
                              [new-:type              :type]
                              [new-:print-type        :print-type]
                              [new-:query-type/args   :query-type/args]
                              [new-:query-type/result :query-type/result]
                              ;[new-field              field]
                              ;[new-super-new          super-new]
                              [new-class              class]))

         (begin-for-syntax
           (define-syntax ~with-tvars
             (pattern-expander
              (syntax-parser
                [(_ (tv tv-stxclass) pat ...)
                 #'{~seq {~var tmp-tv tv-stxclass}
                         {~seq whole-rest (... ...)}
                         {~parse (({~var tv tv-stxclass}) pat ...)
                          ;; rebind tvars:
                          (with-bindings [(tmp-tv.vars (... ...))
                                          (stx-map <shadowed>
                                                   #'(tmp-tv.vars (... ...)))]
                                         ;; rebind occurrences of the tvars within:
                                         (tmp-tv whole-rest (... ...))
                            ;; to (re-)parse:
                            #'(tmp-tv whole-rest (... ...)))}}]))))

         <debug-type-expander>
         
         <:>
         
         <define-type-expander>
         <patch>
         
         (begin-for-syntax
           <remove-ddd>
           <syntax-classes>

           (provide colon))
         
         <define-type>
         <define>
         <lambda>
         <case-lambda>
         <struct>
         <define-struct/exec>
         <ann>
         <cast>
         <unsafe-cast>
         <inst>
         <let>
         <let*>
         <let-values>
         <make-predicate>
         <:type>
         <:print-type>
         <:query-type/args>
         <:query-type/result>
         ;<field>
         <class>
         ;<super-new>
         <other-forms>)]

We can now assemble the modules in this order:

@chunk[<*>
       <module-expander>
       <module-main>
       
       (require 'main)
       (provide (except-out (all-from-out 'main) (for-syntax colon)))]
