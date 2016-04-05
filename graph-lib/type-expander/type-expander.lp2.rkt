#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Extensible types}

We want to have extensible types. Unlike @tc[require] and @tc[provide], which
come with @tc[define-require-syntax] and @tc[define-provide-syntax], and unlike
@tc[match], which comes with @tc[define-match-expander], @tc[typed/racket]
doesn't provide a way to define type expanders (yet).

We re-define the forms @tc[:], @tc[define], @tc[lambda] etc. to equivalents that
expand type expanders defined via our macro @tc[define-type-expander]. Ideally,
this would be handled directly by @tc[typed/racket], which would call the
type-expander itself.

@(table-of-contents)

@section{@racket[prop:type-expander]}

Match expanders are identified by the @tc[prop:type-expander]
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{structure type
 property}, which allows the same identifier to act as a
@tc[prop:rename-transformer], @tc[prop:match-expander] and
@tc[prop:type-expander] for example.

@chunk[<prop:type-expander>
       (define-values (prop:type-expander
                       has-prop:type-expander?
                       get-prop:type-expander-value)
         (make-struct-type-property 'type-expander prop-guard))]

The @tc[prop:type-expander] property should either be the index of a field which
will contain the expander procedure, or directly an expander procedure.

@chunk[<prop-guard>
       (define (prop-guard val struct-type-info-list)
         (cond <prop-guard-field-index>
               <prop-guard-procedure>
               [else
                (raise-argument-error
                 'prop:type-expander-guard
                 (~a "a procedure of arity 1, or an exact non-negative integer "
                     "designating a field index within the structure that "
                     "should contain a procedure of arity 1.")
                 val)]))]

If the value is a field index, it should be within bounds. The
@tc[make-struct-field-accessor] function does that for us, and also returns
an accessor that can be passed directly an instance of the struct.

@chunk[<prop-guard-field-index>
       [(exact-nonnegative-integer? val)
        (make-struct-field-accessor (cadddr struct-type-info-list) val)]]

The expander procedure will take one argument: the piece of syntax corresponding
to the use of the expander. If the property's value is a procedure, we therefore
check that the it's arity includes 1.

When the property value is a field index, we return a field accessor, that when
given the struct instance, will return the actual type expander procedure.
We therfore need to follow the same convention here, by wrapping val in a
single-parameter function.

@chunk[<prop-guard-procedure>
       [(procedure? val)
        (if (arity-includes? (procedure-arity val) 1)
            (λ (_) val)
            (raise-argument-error 'prop:type-expander-guard
                                  "procedure arity should be 1"
                                  val))]]

@subsection{type-expander struct}

We make a simple struct that implements just @tc[prop:type-expander] and nothing
else.

@chunk[<type-expander-struct>
       (struct type-expander (expander-proc)
         #:property prop:type-expander (struct-field-index expander-proc))]

@section{@racket[expand-type]}

@chunk[<apply-type-expander>
       (define (apply-type-expander type-expander-stx stx)
         (let ([type-expander (syntax-local-value type-expander-stx)])
           (((get-prop:type-expander-value type-expander) type-expander) stx)))]

@CHUNK[<bind-type-vars>
       (define (bind-type-vars type-vars [env (make-immutable-free-id-table)])
         (foldl (λ (v acc) (free-id-table-set acc v #f))
                env
                (syntax->list type-vars))
         #;(let ([def-ctx (syntax-local-make-definition-context)]
                 [err-expr #'(λ _ (raise-syntax-error
                                   "Type name used out of context"))])
             (for ([var (syntax-parse type-vars
                          [(v:id …) (syntax->list #'(v …))])])
               (syntax-local-bind-syntaxes (list var) err-expr def-ctx))
             (internal-definition-context-seal def-ctx)
             (internal-definition-context-introduce def-ctx stx 'add)))]

@CHUNK[<let-type-todo>
       (define (let-type-todo id expr env)
         (free-id-table-set env id expr)
         #;(let ([def-ctx (syntax-local-make-definition-context)])
             (syntax-local-bind-syntaxes (list id)
                                         #'(lambda _
                                             (displayln 'expr)
                                             (error "errr"))
                                         def-ctx)
             (internal-definition-context-seal def-ctx)
             (internal-definition-context-introduce def-ctx stx)))]

@CHUNK[<expand-type>
       (define (expand-type stx [env (make-immutable-free-id-table)])
         (define-syntax-class (type-expander env)
           (pattern (~var expander
                          (static has-prop:type-expander? "a type expander"))
                    #:when (not (dict-has-key? env #'expander))
                    #:with code #'expander)
           (pattern expander:id
                    #:attr code (free-id-table-ref env #'expander (λ () #f))
                    #:when (attribute code)))
         (define-syntax-class (type-expander-nested-application env)
           #:attributes (expanded-once)
           (pattern (~and expander-call-stx
                          ((~var expander (type-expander env)) . args))
                    #:with expanded-once
                    (apply-type-expander #'expander.code #'expander-call-stx))
           (pattern ((~var nested-application
                           (type-expander-nested-application env))
                     . args) ;; TODO: test
                    #:with expanded-once
                    #'(nested-application.expanded-once . args))
           (pattern (~and xxx (~datum ~>))
                    #:with expanded-once #'()
                    #:when (display (format "dict =\n  "))
                    #:when (displayln (dict->list env))
                    #:when (displayln #'xxx)
                    #:when (newline)
                    #:when (pretty-write (map syntax-debug-info
                                              (map car (dict->list env))))
                    #:when (pretty-write (syntax-debug-info #'xxx))
                    #:when #f))
         
         (define-syntax-class fa (pattern (~or (~literal ∀) (~literal All))))
         (syntax-parse stx
           [(~datum :) ;; TODO: This is a hack, we should use ~literal.
            #':]
           [(~var expander (type-expander env))
            (expand-type (apply-type-expander #'expander.code #'expander) env)]
           [(~var nested-application (type-expander-nested-application env))
            (expand-type #'nested-application.expanded-once env)]
           ;; TODO: find a more elegant way to write anonymous type expanders
           [(((~literal curry) T Arg1 …) . Args2)
            (expand-type #'(T Arg1 … . Args2) env)]
           ;; TODO: handle the pattern (∀ (TVar ... ooo) T)
           [(∀:fa (TVar:id ...) T:expr)
            #`(∀ (TVar ...)
                 #,(expand-type #'T (bind-type-vars #'(TVar ...) env)))]
           [((~literal Rec) R:id T:expr)
            #`(Rec R #,(expand-type #'T (bind-type-vars #'(R) env)))]
           [((~commit (~datum Let)) bindings T:expr)
            ;; TODO: ~literal instead of ~datum
            (syntax-parse #'bindings
            ;; TODO : for now we only allow aliasing (which means E is an id),
            ;; not on-the-fly declaration of type expanders. This would require
            ;; us to (expand) them.
              [[V:id E:id] ;; TODO: remove the single-binding clause case in Let
               #`#,(expand-type #'T (let-type-todo #'V #'E env))]
              [()
               #`#,(expand-type #'T env)]
              [([V₀:id E₀:id] [Vᵢ:id Eᵢ:id] …)
               #`#,(expand-type #'(Let ([Vᵢ Eᵢ] …) T)
                                (let-type-todo #'V₀ #'E₀ env))])]
           [((~literal quote) T) (expand-quasiquote 'quote 1 env #'T)]
           [((~literal quasiquote) T) (expand-quasiquote 'quasiquote 1 env #'T)]
           [((~literal syntax) T) (expand-quasiquote 'syntax 1 env #'T)]
           [((~literal quasisyntax) T) (expand-quasiquote 'quasisyntax 1 env
                                                          #'T)]
           [((~literal Struct) T)
            #`(Struct #,(expand-type #'T env))]
           [(T TArg ...)
            #`(T #,@(stx-map (λ (a) (expand-type a env)) #'(TArg ...)))]
           [T #'T]))]

@CHUNK[<define-type-expander>
       (define-syntax/parse (define-type-expander (name:id arg:id) . body)
         #`(define-syntax name
             (type-expander #,(syntax/loc stx (λ (arg) . body)))))]

@section{Example type-expanders: quasiquote and quasisyntax}

@CHUNK[<expand-quasiquote>
       (define (expand-quasiquote mode depth env stx)
         (define (wrap t)
           (if (or (eq? mode 'syntax) (eq? mode 'quasisyntax))
               #`(Syntaxof #,t)
               t))
         (define (wrap-quote t)
           (if (or (eq? mode 'syntax) (eq? mode 'quasisyntax))
               #`(Syntaxof (quote #,t))
               #`(quote #,t)))
         (define expand-quasiquote-rec (curry expand-quasiquote mode depth env))
         (syntax-parse stx
           [((~literal quote) T)
            (wrap #`(List #,(wrap-quote #'quote)
                          #,(expand-quasiquote-rec #'T)))]
           [((~literal quasiquote) T)
            (wrap #`(List #,(wrap-quote #'quasiquote)
                          #,(if (eq? mode 'quasiquote)
                                (expand-quasiquote mode (+ depth 1) env #'T)
                                (expand-quasiquote-rec #'T))))]
           [((~literal unquote) T)
            (if (eq? mode 'quasiquote)
                (if (= depth 1)
                    (expand-type #'T env)
                    (wrap #`(List #,(wrap-quote #'unquote)
                                  #,(expand-quasiquote mode (- depth 1) env
                                                       #'T))))
                (wrap #`(List #,(wrap-quote #'unquote)
                              #,(expand-quasiquote-rec #'T))))]
           [((~literal syntax) T)
            (wrap #`(List #,(wrap-quote #'quote)
                          #,(expand-quasiquote-rec #'T)))]
           [((~literal quasisyntax) T)
            (wrap #`(List #,(wrap-quote #'quasisyntax)
                          #,(if (eq? mode 'quasisyntax)
                                (expand-quasiquote mode (+ depth 1) env #'T)
                                (expand-quasiquote-rec #'T))))]
           [((~literal unsyntax) T)
            (if (eq? mode 'quasisyntax)
                (if (= depth 1)
                    (expand-type #'T env)
                    (wrap #`(List #,(wrap-quote #'unsyntax)
                                  #,(expand-quasiquote mode (- depth 1) env
                                                       #'T))))
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
            (wrap #`(List #,@(stx-map expand-quasiquote-rec #'(T ...))))]
           [(T ... U . S)
            (wrap #`(List* #,@(stx-map expand-quasiquote-rec #'(T ... U S))))]
           [#(T ...)
            (wrap #`(Vector #,@(stx-map expand-quasiquote-rec #'(T ...))))]
           [#&T (wrap #`(Boxof #,(expand-quasiquote-rec #'T)))]
           ; TODO: Prefab with #s(prefab-struct-key type ...)
           [T:id (wrap #''T)]
           [T #:when (string? (syntax-e #'T)) (wrap #'T)]
           [T:number (wrap #'T)]
           [T:keyword (wrap #''T)]
           [T:char (wrap #'T)]
           [#t (wrap #'True)]
           [#t (wrap #'False)]
           [_ (raise-syntax-error 'expand-quasiquote
                                  (format "Unknown quasiquote contents: ~a" stx)
                                  stx)]))]

@section{Overloading @racket[typed/racket] forms}

Througout this section, we provide alternative definitions of the
@tc[typed/racket] forms @tc[:], @tc[lambda], @tc[define], @tc[struct], @tc[ann],
@tc[inst]… . We write these definitions with @tc[syntax-parse], using the syntax
classes defined in section @secref{type-expander|syntax-classes}.

Most of the time, we will use the experimental @tc[template] macro from
@tc[syntax/parse/experimental/template] which allows more concise code than the
ususal @code{#'()} and @code{#`()}. In order to expand types and bind type
variables in the result, we define two template metafunctions:

@chunk[<template-metafunctions>
       (define-template-metafunction (tmpl-expand-type stx)
         (syntax-parse stx
           [(_ () t) (expand-type #'t)]
           [(_ (tvar …) t) (expand-type #'t (bind-type-vars #'(tvar …)))]))]

@subsection{@racket[:]}

We provide a new definition for the @tc[:] operator, which calls the old one
after expanding the type argument.

@CHUNK[<:>
       (define-syntax/parse (new-: x:id t:expr)
         #`(: x #,(expand-type #'t)))]

@subsection[#:tag "type-expander|syntax-classes"]{syntax classes}

The syntax classes from @tc[typed-racket/base-env/annotate-classes] match
against the @tc[:] literal. Since we provide a new definition for it, the syntax
class doesn't match code using our definition of @tc[:]. We therefore can't use
the original implementations of @tc[:curried-formals] and @tc[lambda-formals],
and instead have to roll out our own versions.

We take that as an opportunity to expand the types directly from the syntax
classes using @tc[#:with], instead of doing that inside the macros that use
them.

@CHUNK[<syntax-classes>
       (define-syntax-class colon
         (pattern (~or (~literal new-:) (~literal :))))
       
       (define-splicing-syntax-class (new-maybe-kw-type-vars)
         #:attributes (vars maybe)
         (pattern kw+vars:lambda-type-vars
                  #:with vars #'kw+vars.type-vars
                  #:with maybe #'kw+vars)
         (pattern (~seq)
                  #:with vars #'()
                  #:attr maybe #f))
       
       (define-splicing-syntax-class (new-maybe-type-vars)
         #:attributes (vars maybe)
         (pattern vars:type-variables
                  #:with maybe #'vars)
         (pattern (~seq)
                  #:with vars #'()
                  #:attr maybe #f))
       
       (define-splicing-syntax-class (new-kw-formal tvars)
         #:attributes ([expanded 1])
         (pattern (~seq kw:keyword id:id)
                  #:with (expanded ...) #'(kw id))
         (pattern (~seq kw:keyword [id:id
                                    (~optional (~seq :colon type:expr))
                                    (~optional default:expr)])
                  #:with tvars tvars
                  #:with (expanded ...)
                  (template (kw [id (?@ : (tmpl-expand-type tvars type))
                                 (?? default)]))))
       
       (define-splicing-syntax-class (new-mand-formal tvars)
         #:attributes ([expanded 1])
         (pattern id:id
                  #:with (expanded ...) #'(id))
         (pattern [id:id :colon type:expr]
                  #:with tvars tvars
                  #:with (expanded ...)
                  (template ([id : (tmpl-expand-type tvars type)])))
         (pattern (~var kw (new-kw-formal tvars))
                  #:with (expanded ...) #'(kw.expanded ...)))
       
       (define-splicing-syntax-class (new-opt-formal tvars)
         #:attributes ([expanded 1])
         (pattern [id:id (~optional (~seq :colon type:expr)) default:expr]
                  #:with tvars tvars
                  #:with (expanded ...)
                  (template ([id (?? (?@ : (tmpl-expand-type tvars type)))
                              default])))
         (pattern (~var kw (new-kw-formal tvars))
                  #:with (expanded ...) #'(kw.expanded ...)))
       
       (define-syntax-class (new-rest-arg tvars)
         #:attributes ([expanded 0])
         (pattern rest:id
                  #:with expanded #'rest)
         (pattern (rest:id
                   :colon type:expr
                   (~or (~and x* (~describe "*" (~or (~datum *) (~datum ...*))))
                        (~seq (~datum ...) bound:expr)))
                  #:with tvars tvars
                  #:with expanded
                  (template (rest : (tmpl-expand-type tvars type)
                                  (?? x* (?@ (... ...) (tmpl-expand-type
                                                        tvars bound)))))))
       
       (define-syntax-class (new-lambda-formals tvars)
         (pattern (~or ((~var mand (new-mand-formal tvars)) ...
                        (~var opt (new-opt-formal tvars)) ...
                        . (~var rest (new-rest-arg tvars)))
                       ((~var mand (new-mand-formal tvars)) ...
                        (~var opt (new-opt-formal tvars)) ...))
                  ;; TODO: once template supports ?? in tail position, use it.
                  #:with expanded #`(mand.expanded ...
                                     ...
                                     opt.expanded ...
                                     ...
                                     . #,(if (attribute rest)
                                             #'rest.expanded
                                             #'()))))
       
       (define-syntax-class (new-curried-formals tvars)
         (pattern (f:id . (~var args (new-lambda-formals tvars)))
                  #:with expanded #'(f . args.expanded))
         (pattern ((~var lhs (new-curried-formals tvars))
                   . (~var args (new-lambda-formals tvars)))
                  #:with expanded #'(lhs.expanded . args.expanded)))
       
       (define-splicing-syntax-class (new-optionally-annotated-name tvars)
         (pattern (~seq name:id (~optional (~seq :colon type:expr)))
                  #:with tvars tvars
                  #:with expanded
                  (template (name
                             (?? (?@ : (tmpl-expand-type tvars type)))))))
       
       (define-syntax-class (new-name-or-parenthesised-annotated-name tvars)
         (pattern name:id
                  #:with expanded #'name)
         (pattern [id:id :colon type:expr]
                  #:with tvars tvars
                  #:with expanded
                  (template [id : (tmpl-expand-type tvars type)])))]

@subsection{@racket[define-type]}

@chunk[<define-type>
       (define-syntax (new-define-type stx)
         (syntax-parse stx
           [(_ (~or name:id (name:id TVar ...)) type . rest)
            (template
             (define-type (?? (name TVar ...) name)
               (tmpl-expand-type (?? (TVar ...) ()) type)
               . rest))]))]

@subsection{@racket[define]}

@chunk[<define>
       (define-syntax (new-define stx)
         (syntax-parse stx
           [(_ tvars:new-maybe-kw-type-vars
               (~or v:id
                    (~var formals (new-curried-formals #'tvars.vars)))
               (~optional (~seq :colon type))
               e ...)
            (template
             (define (?@ . tvars) (?? v formals.expanded)
               (?? (?@ : (tmpl-expand-type tvars.vars type)))
               e ...))]))]

@subsection{@racket[lambda]}

@CHUNK[<lambda>
       (define-syntax (new-lambda stx)
         (syntax-parse stx
           [(_ tvars:new-maybe-kw-type-vars
               (~var args (new-lambda-formals #'tvars.vars))
               (~optional (~seq :colon ret-type))
               e ...)
            (template (lambda (?@ . tvars) args.expanded
                        (?? (?@ : (tmpl-expand-type tvars.vars ret-type)))
                        e ...))]))]

@subsection{@racket[struct]}

@chunk[<struct>
       (define-syntax (new-struct stx)
         (syntax-parse stx
           [(_ tvars:new-maybe-type-vars
               (~and (~seq name+parent …) (~or (~seq name:id)
                                               (~seq name:id parent:id)))
               ([field:id :colon type:expr] ...)
               . rest)
            (template (struct (?? tvars.maybe) name (?? parent)
                        ([field : (tmpl-expand-type tvars.vars type)] ...)
                        . rest))]))]

@subsection{@racket[define-struct/exec]}

@chunk[<define-struct/exec>
       (define-syntax (new-define-struct/exec stx)
         (syntax-parse stx
           [(_ (~and name+parent (~or name:id [name:id parent:id]))
               ([field:id (~maybe :colon type:expr)] ...)
               [proc :colon proc-type])
            (template (define-struct/exec name+parent
                        ([field (?? (?@ : (tmpl-expand-type () type)))] ...)
                        [proc : (tmpl-expand-type () proc-type)]))]))]

@subsection{@racket[ann]}

@chunk[<ann>
       (define-syntax/parse (new-ann value:expr (~optional :colon) type:expr)
         (template (ann value (tmpl-expand-type () type))))]

@subsection{@racket[inst]}

@chunk[<inst>
       (define-syntax/parse (new-inst v
                                      (~optional :colon) t ...
                                      (~optional (~seq last (~datum ...) b:id)))
         (template (inst v (tmpl-expand-type () t) ...
                         (?? (?@ (tmpl-expand-type () last)
                                 (... ...) b)))))]

@subsection{@racket[let]}

@chunk[<let>
       (define-syntax/parse
         (new-let
          (~optional (~seq loop:id (~optional (~seq :colon return-type:expr))))
          tvars:new-maybe-kw-type-vars
          ([(~var name (new-optionally-annotated-name #'tvars.vars))
            e:expr] ...)
          . rest)
         (template
          (let (?? (?@ loop (?? (?@ : (tmpl-expand-type tvars.vars
                                                        return-type)))))
            (?@ . tvars)
            ([(?@ . name.expanded) e] ...)
            . rest)))]

@subsection{@racket[let*]}

@chunk[<let*>
       (define-syntax/parse
         (new-let*
          ([(~var name (new-optionally-annotated-name #'()))
            e:expr] ...)
          . rest)
         (template
          (let* ([(?@ . name.expanded) e] ...)
            . rest)))]

@subsection{@racket[let-values]}

@chunk[<let-values>
       (define-syntax/parse
         (new-let-values
          ([((~var name (new-name-or-parenthesised-annotated-name #'())) ...)
            e:expr] ...)
          . rest)
         (template
          (let-values ([(name.expanded ...) e] ...)
            . rest)))]

@subsection{@racket[make-predicate]}

@chunk[<make-predicate>
       (define-syntax/parse (new-make-predicate type:expr)
         (template (make-predicate (tmpl-expand-type () type))))]

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
                     (format "~a not implemented yet for type-expander" 'name)))
                  (provide (rename-out [tmp name])))
                ...)]))
       
       (missing-forms
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
        require-typed-struct-legacy
        require-typed-struct
        require/typed-legacy
        require/typed
        require/typed/provide
        require-typed-struct/provide
        cast
        ;make-predicate
        define-predicate
        ;; prims.rkt
        define-type-alias
        define-new-subtype
        define-typed-struct
        define-typed-struct/exec
        ;ann
        ;inst
        ;:
        define-struct:
        define-struct
        ;struct
        struct:
        λ:
        ;lamdba
        ;λ
        ;define
        ;let
        ;let*
        letrec
        ;let-values
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
        #|define-struct/exec|#)]

@section{Future work}

We have not implemented alternative type-expanding definitions for all the
@tc[typed/racket] forms, as noted in @secref{type-expander|other-forms}.

Integrating the type-expander directly into typed/racket would avoid the need to
provide such definitions, and allow using type-expanders in vanilla
@tc[typed/racket], instead of having to @tc[require] this library. However, the
code wrapping the @tc[typed/racket] forms could be re-used by other libraries
that alter the way @tc[typed/racket] works, so implementing the remaining forms
could still be useful.

Also, we would need to provide a @tc[syntax-local-type-introduce] function,
similar to the @tc[syntax-local-match-introduce] function provided by @tc[match]
for example.

@section{Conclusion}

When an identifier is @tc[require]d from another module, it is not the same as
the one visible within the defining module. This is a problem for @tc[:],
because we match against it in our syntax classes, using @tc[(~literal :)], but
when it is written in another module, for example @tc[(define foo : Number 42)],
it is not the same identifier as the one used by original definition of @tc[:],
and therefore the @tc[(~literal :)] won't match. I suspect that issue to be due
to contract wrappers added by @tc[typed/racket].

To get around that problem, we define @tc[:] in a separate module, and
@tc[require] it in the module containing the syntax classes:

@chunk[<module-colon>
       (module colon typed/racket
         (require (for-syntax racket
                              syntax/parse)
                  phc-toolkit)
         (require (for-syntax (submod ".." expander)))
         
         (provide new-:)
         
         <:>)]

Since our @tc[new-:] macro needs to call the @tc[type-expander], and the other
forms too, we can't define @tc[type-expander] in the same module as these forms,
it needs to be either in the same module as @tc[new-:], or in a separate module.
Additionally, @tc[expand-type] needs to be required @tc[for-syntax] by the
forms, but needs to be @tc[provide]d too, so it is much easier if it is defined
in a separate module (that will be used only by macros, so it will be written in
@tc[racket], not @tc[typed/racket]).

@chunk[<module-expander>
       (module expander racket
         (require racket
                  syntax/parse
                  racket/format
                  syntax/id-table
                  (submod phc-toolkit untyped))
         
         (require (for-template typed/racket))
         
         (provide prop:type-expander
                  type-expander
                  apply-type-expander
                  bind-type-vars
                  expand-type)
         
         <prop-guard>
         <prop:type-expander>
         <type-expander-struct>
         
         <apply-type-expander>
         <expand-quasiquote>
         <bind-type-vars>
         <let-type-todo>
         <expand-type>)]

We can finally define the overloaded forms, as well as the extra
@tc[<define-type-expander>].

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax racket
                              racket/syntax
                              syntax/parse
                              syntax/parse/experimental/template
                              (submod phc-toolkit untyped))
                  phc-toolkit)
         
         (require (submod ".." expander))
         (require (for-syntax (submod ".." expander)))
         (require (for-syntax typed-racket/base-env/annotate-classes))
         (require (submod ".." colon))
         
         (provide prop:type-expander
                  expand-type
                  define-type-expander
                  (rename-out [new-: :]
                              [new-define-type define-type]
                              [new-define define]
                              [new-lambda lambda]
                              [new-lambda λ]
                              [new-struct struct]
                              [new-define-struct/exec define-struct/exec]
                              [new-ann ann]
                              [new-inst inst]
                              [new-let let]
                              [new-let* let*]
                              [new-let-values let-values]
                              [new-make-predicate make-predicate]))
         
         <define-type-expander>
         
         (begin-for-syntax
           <template-metafunctions>
           <syntax-classes>
           
           (provide colon))
         
         <define-type>
         <define>
         <lambda>
         <struct>
         <define-struct/exec>
         <ann>
         <inst>
         <let>
         <let*>
         <let-values>
         <make-predicate>
         <other-forms>)]

We can now assemble the modules in this order:

@chunk[<*>
       (begin
         <module-expander>
         <module-colon>
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main)))]
