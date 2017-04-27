#lang hyper-literate typed/racket/base #:no-require-lang #:no-auto-require
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
           (for-label "type-expander.hl.rkt"
                      (submod "type-expander.hl.rkt" expander)
                      (subtract-in typed/racket/base
                                   "type-expander.hl.rkt")
                      (subtract-in racket
                                   typed/racket/base
                                   "type-expander.hl.rkt")
                      typed/racket/unsafe
                      racket/format
                      racket/syntax
                      syntax/stx
                      syntax/parse
                      syntax/parse/experimental/template
                      syntax/id-table)))

@title[#:style manual-doc-style
       #:tag "ty-xp-more"
       #:tag-prefix "type-expander/ty-xp-more"]{Some example type expanders}

@(chunks-toc-prefix
  '("(lib type-expander/scribblings/type-expander-implementation.scrbl)"
    "type-expander/ty-xp-more"))

@section{Example type expanders: quasiquote and quasisyntax}

We define type expanders for @racket[quote], @racket[quasiquote],
@racket[syntax] and @racket[quasisyntax]:

The next four special forms are implemented as type expanders with
@tc[patch-type-expander] because redefining their name (@tc[quote],
@tc[quasiquote], @tc[syntax] and @tc[quasisyntax]) would conflict with
existing identifiers. @racket[patch-type-expander] uses a global persistant
(across modules) for-syntax mutable table, which associates identifiers to
type-expanders. @note{ @racketmodname[typed/racket] works in that way by
 associating data (their type) to existing identifiers. The
 @racketmodname[mutable-match-lambda] library on the other hand allows adding
 behaviour to an identifier after it is defined, but relies on some level of
 cooperation from that identifier, which may be less practical for built-in
 identifiers like @racket[quote].} Relying on an external data structure to
associate information with identifiers makes it possible to overload the
meaning of @tc[quote] or @tc[curry] when used as a type expander, without
having to alter their original definition. Another option would be to provide
overloaded versions of these identifiers, to shadow those imported by the
@litchar{#lang} module. This would however cause conflicts for @tc[curry] when
@tc[racket/function] is explicitly required (instead of being required
implicitly by @racket[#,hash-lang #,(racketmodname racket)], for example.

@chunk[<quotes>
       (patch-type-expander quote
         (λ (stx)
           (syntax-case stx ()
             [(_ T)
              (expand-quasiquote 'quote 1 #'T)])))]

@chunk[<quotes>
       (patch-type-expander quasiquote
         (λ (stx)
           (syntax-case stx ()
             [(_ T)
              (expand-quasiquote 'quasiquote 1 #'T)])))]

@chunk[<quotes>
       (patch-type-expander syntax
         (λ (stx)
           (syntax-case stx ()
             [(_ T)
              (expand-quasiquote 'syntax 1 #'T)])))]

@chunk[<quotes>
       (patch-type-expander quasisyntax
         (λ (stx)
           (syntax-case stx ()
             [(_ T)
              (expand-quasiquote 'quasisyntax 1 #'T)])))]

Their implementation is factored out into the @tc[expand-quasiquote]
for-syntax function. It is a reasonably complex showcase of this library's
functionality. @racketmodname[typed/racket] allows the use of @tc[quote] to
describe a type which contains a single inhabitant, the quoted datum. For
example, @tc[(define-type foo '(a b (1 2 3) c))] declares a type @tc[foo]
which is equivalent to @tc[(List 'a 'b (List 1 2 3) 'c)].

We build upon that idea to allow the use of @tc[syntax], 
@tc[quasiquote] and @tc[quasisyntax]. Both @tc[syntax] and 
@tc[quasisyntax] wrap each s-expression within the quoted
datum with @tc[Syntaxof], which avoids the otherwise tedious
declaration of the type for a piece of syntax. Both 
@tc[quasiquote] and @tc[quasisyntax] allow escaping the
quoted datum (using @tc[unquote] and @tc[unsyntax],
respectively). A later version of this library could
support @tc[unquote-splicing] and @tc[unsyntax-splicing].

Using this type-expander, one can write 
@racketblock[(define-type bar `(a ,Symbol (1 ,(U Number String) 3) c))]
The above declaration gets expanded to: 
@racketblock[(define-type bar (List 'a Symbol (List 1 (U Number String) 3) 'c))]

The implementation of @tc[expand-quasiquote] recursively
traverses the type expression. The @tc[mode] argument
can be one of @tc['quote], @tc['quasiquote], @tc['syntax] or
@tc['quasisyntax]. It is used to determine whether to wrap
parts of the type with @tc[Syntaxof] or not, and to know
which identifier escapes the quoting (@tc[unquote] or 
@tc[unsyntax]). The @tc[depth] argument keeps track of the
quoting depth: in Racket @tc[`(foo `(bar ,baz))] is
equivalent to 
@tc[(list 'foo (list 'quasiquote (list 'bar (list 'unquote 'baz))))]
(two levels of @tc[unquote] are required to escape the two
levels of @tc[quasiquote]), so we want the type to be 
@tc[(List 'foo (List 'quasiquote (List 'bar (List 'unquote 'baz))))].

@CHUNK[<expand-quasiquote>
       (define (list*->list l)
         (if (pair? l)
             (cons (car l) (list*->list (cdr l)))
             (list l)))
       (define (expand-quasiquote mode depth stx)
         (define (wrap t)
           (if (or (eq? mode 'syntax) (eq? mode 'quasisyntax))
               #`(Syntaxof #,t)
               t))
         (define (wrap-quote t)
           (if (or (eq? mode 'syntax) (eq? mode 'quasisyntax))
               #`(Syntaxof (No-Expand (quote #,t)))
               #`(No-Expand (quote #,t))))
         (define expand-quasiquote-rec (curry expand-quasiquote mode depth))
         (syntax-parse stx
           [((~literal quote) T)
            (wrap #`(List #,(wrap-quote #'quote)
                          #,(expand-quasiquote-rec #'T)))]
           [((~literal quasiquote) T)
            (wrap #`(List #,(wrap-quote #'quasiquote)
                          #,(if (eq? mode 'quasiquote)
                                (expand-quasiquote mode (+ depth 1) #'T)
                                (expand-quasiquote-rec #'T))))]
           [((~literal unquote) T)
            (if (eq? mode 'quasiquote)
                (if (= depth 1)
                    (expand-type #'T) ;; TODO: applicable? !!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    (wrap #`(List #,(wrap-quote #'unquote)
                                  #,(expand-quasiquote mode (- depth 1) #'T))))
                (wrap #`(List #,(wrap-quote #'unquote)
                              #,(expand-quasiquote-rec #'T))))]
           [((~literal syntax) T)
            (wrap #`(List #,(wrap-quote #'quote)
                          #,(expand-quasiquote-rec #'T)))]
           [((~literal quasisyntax) T)
            (wrap #`(List #,(wrap-quote #'quasisyntax)
                          #,(if (eq? mode 'quasisyntax)
                                (expand-quasiquote mode (+ depth 1) #'T)
                                (expand-quasiquote-rec #'T))))]
           [((~literal unsyntax) T)
            (if (eq? mode 'quasisyntax)
                (if (= depth 1)
                    (expand-type #'T) ;; TODO: applicable? !!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    (wrap #`(List #,(wrap-quote #'unsyntax)
                                  #,(expand-quasiquote mode (- depth 1) #'T))))
                (wrap #`(List #,(wrap-quote #'unsyntax)
                              #,(expand-quasiquote-rec #'T))))]
           ;; TODO For lists, we should consider the cases where syntax-e gives
           ;; a pair vs the cases where it gives a list.
           [(T . U)
            #:when (syntax? (cdr (syntax-e stx)))
            (wrap #`(Pairof #,(expand-quasiquote-rec #'T)
                            #,(expand-quasiquote-rec #'U)))]
           [() (wrap #'Null)]
           [(T ...)
            #:when (list? (syntax-e stx))
            (wrap #`(List #,@(stx-map expand-quasiquote-rec #'(T ...))))]
           [whole
            #:when (pair? (syntax-e #'whole))
            #:with (T ... S) (list*->list (syntax-e #'whole))
            (wrap #`(List* #,@(stx-map expand-quasiquote-rec #'(T ... S))))]
           [#(T ...)
            (wrap #`(Vector #,@(stx-map expand-quasiquote-rec #'(T ...))))]
           [#&T (wrap #`(Boxof #,(expand-quasiquote-rec #'T)))]
           ; TODO: Prefab with #s(prefab-struct-key type ...)
           [T:id (wrap #'(No-Expand (quote T)))]
           [T #:when (string? (syntax-e #'T)) (wrap #'T)]
           [T:number (wrap #'T)]
           [T:keyword (wrap #'(No-Expand (quote T)))]
           [T:char (wrap #'T)]
           [#t (wrap #'True)]
           [#t (wrap #'False)]
           [_ (raise-syntax-error 'expand-quasiquoste
                                  (format "Unknown quasiquote contents: ~a" stx)
                                  stx)]))]

@section{Implementation of the @racket[Let*] special type expander form}

The @racket[Let*] special form is implemented in terms of @racket[Let],
binding each variable in turn:

@chunk[<Let*>
       (define-type-expander (Let* stx)
         (syntax-case stx ()
           [(me ([var val] . rest) τ)
            (with-syntax ([L (datum->syntax #'here 'Let #'me #'me)]
                          [L* (datum->syntax #'here 'Let* #'me #'me)])
              #'(L ([var val])
                   (L* rest
                       τ)))]
           [(_ () τ) #'τ]))]

@section{curry}

The @tc[curry] special form takes a type expander (or a polymorphic type) and
some arguments. The whole form should appear in the first position of its
containing form, which contains more arguments, or be bound with a
@racket[Let] or @racket[Letrec]. @tc[curry] appends the arguments in the outer
form to the whole inner form, and expands the result. This really should be
implemented as a type expander so that the partially-applied expander or
polymorphic type can be bound using @tc[Let], for example, but for now it is
hardcoded here.
       
@chunk[<curry>
       (patch-type-expander curry
         (λ (stx)
           (syntax-case stx ()
             [(_ T Arg1 ...)
              #'(Λ (_ . Args2) #'(T Arg1 ... . Args2))])))]

@section{Putting it all together}

@chunk[<*>
       (require "type-expander.hl.rkt"
                "identifiers.rkt"
                racket/function
                (for-syntax racket/base
                            (only-in racket/base [... …])
                            (submod "type-expander.hl.rkt" expander)
                            syntax/parse
                            syntax/stx
                            racket/function
                            racket/match))
       (provide Let*)

       <Let*>

       (begin-for-syntax <expand-quasiquote>)
       <quotes>

       <curry>]