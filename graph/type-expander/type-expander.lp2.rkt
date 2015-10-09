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
@tech{structure type property}, which allows the same identifier to act as a
@tc[prop:rename-transformer], @tc[prop:match-expander] and
@tc[prop:type-expander] for example.

@chunk[<prop:type-expander>
       (define-values (prop:type-expander
                       has-prop:type-expander?
                       get-prop:type-expander-value)
         (make-struct-type-property 'type-expander prop-guard))]

The prop:type-expander property should either be the index of a field which will
contain the expander procedure, or directly an expander procedure.

@chunk[<prop-guard>
       (define (prop-guard val struct-type-info-list)
         (cond <prop-guard-field-index>
               <prop-guard-procedure>
               [else
                (raise-argument-error
                 'prop:type-expander-guard
                 (string-append
                  "an exact non-negative integer designating a field index "
                  "within the structure that should contain a procedure of "
                  "arity 1, or a procedure of arity 1.")
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
       (define (bind-type-vars type-vars stx)
         (let ([def-ctx (syntax-local-make-definition-context)]
               [err-expr #'(λ _ (raise-syntax-error
                                 "Type name used out of context"))])
           (for ([var (syntax->list type-vars)])
             (syntax-local-bind-syntaxes (list var) err-expr def-ctx))
           (internal-definition-context-seal def-ctx)
           (internal-definition-context-introduce def-ctx stx)))]

@CHUNK[<expand-type>
       (define (expand-type stx)
         (define-syntax-class type-expander
           (pattern (~var expander
                          (static has-prop:type-expander? "a type expander"))))
         (define-syntax-class fa (pattern (~or (~literal ∀) (~literal All))))
         (syntax-parse stx
           [:type-expander
            (expand-type (apply-type-expander #'expander #'expander))]
           [(~and expander-call-stx (:type-expander . args))
            (expand-type (apply-type-expander #'expander #'expander-call-stx))]
           ;; TODO: handle the pattern (∀ (TVar ... ooo) T)
           [(∀:fa (TVar ...) T)
            #`(∀ (TVar ...) #,(expand-type (bind-type-vars #'(TVar ...) #'T)))]
           [((~literal Rec) R T)
            #`(Rec R #,(expand-type (bind-type-vars #'(R) #'T)))]
           [((~literal quote) T) (expand-quasiquote 'quote 1 #'T)]
           [((~literal quasiquote) T) (expand-quasiquote 'quasiquote 1 #'T)]
           [((~literal syntax) T) (expand-quasiquote 'syntax 1 #'T)]
           [((~literal quasisyntax) T) (expand-quasiquote 'quasisyntax 1 #'T)]
           [(T TArg ...)
            #`(T #,@(stx-map expand-type #'(TArg ...)))]
           [T #'T]))]

@CHUNK[<define-type-expander>
       (define-syntax/parse (define-type-expander (name:id arg:id) . body)
         #'(define-syntax name (type-expander (λ (arg) . body))))]

@subsection{Tests for @racket[expand-type]}

@CHUNK[<test-expand-type>
       (require (for-syntax typed/rackunit
                            syntax/parse))
       
       (define-syntax (test-expander stx)
         (syntax-parse stx
           [(_ type expanded-type)
            (check-equal? (syntax->datum (expand-type #'type))
                          (syntax->datum #'expanded-type))
            #'(values)]))]

Simple identity expander test, with a different case when used just as an
identifier.

@CHUNK[<test-expand-type>
       (define-type-expander (id stx)
         (syntax-case stx ()
           [(_ t) #'t]
           [x #'(∀ (A) (→ A A))]))
       
       (test-expander (id Number) Number)
       (test-expander id (∀ (A) (→ A A)))]

@CHUNK[<test-expand-type>
       (define-type-expander (double stx)
         (syntax-case stx ()
           [(_ t) #'(id (Pairof (id t) t))]))
       
       (test-expander (∀ (A) (→ A (id (double (id A)))))
                      (∀ (A) (→ A (Pairof A A))))]

Shadowing and @tc[∀] variables:

@CHUNK[<test-expand-type>
       (test-expander (∀ (id) (→ id))
                      (∀ (id) (→ id)))
       (test-expander (∀ (id2) (→ id))
                      (∀ (id2) (→ (∀ (A) (→ A A)))))]

@CHUNK[<test-expand-type>
       (define-type-expander (Repeat stx)
         (syntax-case stx ()
           [(_ t n) #`(List #,@(map (λ (x) #'t)
                                    (range (syntax->datum #'n))))]))
       
       (test-expander (Repeat Number 5)
                      (List Number Number Number Number Number))]

@CHUNK[<test-expand-type>
       (: count-five-more (→ Number (Repeat Number 5)))
       (define (count-five-more x)
         (list (+ x 1) (+ x 2) (+ x 3) (+ x 4) (+ x 5)))
       
       (check-equal? (count-five-more 3)
                     '(4 5 6 7 8))
       (check-equal? (ann (count-five-more 15) (Repeat Number 5))
                     '(16 17 18 19 20))]

@section{Example type-expanders: quasiquote and quasisyntax}

@CHUNK[<expand-quasiquote>
       (define (expand-quasiquote mode depth stx)
         (define (wrap t)
           (if (or (eq? mode 'syntax) (eq? mode 'quasisyntax))
               #`(Syntaxof #,t)
               t))
         (define (wrap-quote t)
           (if (or (eq? mode 'syntax) (eq? mode 'quasisyntax))
               #`(Syntaxof (quote #,t))
               #`(quote #,t)))
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
                    (expand-type #'T)
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
                    (expand-type #'T)
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
                                  "Unknown quasiquote contents"
                                  stx)]))]

@section{Overloading @racket[typed/racket] forms}

Througout this section, we provide alternative definitions of the
@tc[typed/racket] forms @tc[:], @tc[lambda], @tc[define], @tc[struct], @tc[ann],
@tc[inst]… . We write these definitions with @tc[syntax-parse], using the syntax
classes defined in section @secref{syntax-classes}.

Most of the time, we will use the experimental @tc[template] macro from
@tc[syntax/parse/experimental/template] which allows more concise code than the
ususal @code{#'()} and @code{#`()}. In order to expand types and bind type
variables in the result, we define two template metafunctions:

@chunk[<template-metafunctions>
       (define-template-metafunction (template-expand-type stx)
         (syntax-parse stx
           [(_ () t) (expand-type #'t)]
           [(_ tvars t) (expand-type (bind-type-vars #'tvars #'t))]))]

@subsection{@racket[:]}

We provide a new definition for the @tc[:] operator, which calls the old one
after expanding the type argument.

@CHUNK[<:>
       (define-syntax/parse (new-: x:id t:expr)
         #`(: x #,(expand-type #'t)))]

@CHUNK[<test-:>
       (: c0 `(2 "abc" #,,(Pairof (U 'x 'y) (U 'y 'z)) #(1 "b" x) d))
       (define c0 '(2 "abc" #,(x . z) #(1 "b" x) d))]

@subsection[#:tag "syntax-classes"]{syntax classes}

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
         (pattern (~literal new-:)))
       
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
                  (template (kw [id (?@ : (template-expand-type tvars type))
                                 (?? default)]))))
       
       (define-splicing-syntax-class (new-mand-formal tvars)
         #:attributes ([expanded 1])
         (pattern id:id
                  #:with (expanded ...) #'(id))
         (pattern [id:id :colon type:expr]
                  #:with tvars tvars
                  #:with (expanded ...)
                  (template ([id : (template-expand-type tvars type)])))
         (pattern (~var kw (new-kw-formal tvars))
                  #:with (expanded ...) #'(kw.expanded ...)))
       
       (define-splicing-syntax-class (new-opt-formal tvars)
         #:attributes ([expanded 1])
         (pattern [id:id (~optional (~seq :colon type:expr)) default:expr]
                  #:with tvars tvars
                  #:with (expanded ...)
                  (template ([id (?@ : (template-expand-type tvars type))
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
                  (template (rest : (template-expand-type tvars type)
                                  (?? x* (?@ (... ...) (template-expand-type
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
                             (?? (?@ : (template-expand-type tvars type)))))))
       
       (define-syntax-class (new-name-or-parenthesised-annotated-name tvars)
         (pattern name:id
                  #:with expanded #'name)
         (pattern [id:id :colon type:expr]
                  #:with tvars tvars
                  #:with expanded
                  (template [id : (template-expand-type tvars type)])))]

@subsection{@racket[define-type]}

@chunk[<define-type>
       (define-syntax (new-define-type stx)
         (syntax-parse stx
           [(_ (~or name:id (name:id TVar ...)) type . rest)
            (template
             (define-type (?? (name TVar ...) name)
               (template-expand-type (?? (TVar ...) ()) type)
               . rest))]))]

@chunk[<test-define-type>
       (let ()
         (define-type-expander (Repeat stx)
           (syntax-case stx ()
             [(_ t n) #`(List #,@(map (λ (x) #'t)
                                      (range (syntax->datum #'n))))]))
         
         (define-type R5 (Repeat Number 5))
         (check-equal? (ann '(1 2 3 4 5) R5) '(1 2 3 4 5)))]

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
               (?? (?@ : (template-expand-type tvars.vars type)))
               e ...))]))]

@CHUNK[<test-define>
       (define d0
         : `(2 "abc" #,,(Pairof (U 'x 'y) (U 'y 'z)) #(1 "b" x) d)
         '(2 "abc" #,(x . z) #(1 "b" x) d))
       (check-equal? (ann d0 (List 2
                                   "abc"
                                   (List 'unsyntax (Pairof (U 'x 'y) (U 'y 'z)))
                                   (Vector 1 "b" 'x) 'd))
                     '(2 "abc" (unsyntax (x . z)) #(1 "b" x) d))
       
       (: d1 (→ Number (→ Number Number)))
       (define ((d1 [x : Number]) [y : Number]) : Number (+ x y))
       (check-equal? (ann ((d1 2) 3) Number) 5)
       
       (: d2 (→ Number (→ Number Number)))
       (define ((d2 [x : Number]) [y : Number]) (+ x y))
       (check-equal? (ann ((d2 3) 4) Number) 7)
       
       (define #:∀ (T) ((d3 [x : T]) [y : T]) : (Pairof T T) (cons x y))
       (check-equal? (ann ((d3 'x) 'y) (Pairof Symbol Symbol)) '(x . y))]

@subsection{@racket[lambda]}

@CHUNK[<lambda>
       (define-syntax (new-lambda stx)
         (syntax-parse stx
           [(_ tvars:new-maybe-kw-type-vars
               (~var args (new-lambda-formals #'tvars.vars))
               (~optional (~seq :colon ret-type))
               e ...)
            (template (lambda (?@ . tvars) args.expanded
                        (?? (?@ : (template-expand-type tvars.vars ret-type)))
                        e ...))]))]

@CHUNK[<test-lambda>
       (check-equal? ((ann (lambda ([x : Number]) : Number (* x 2))
                           (→ Number Number))
                      3)
                     6)
       (check-equal? ((ann (λ ([x : Number]) : Number (* x 2))
                           (→ Number Number))
                      3)
                     6)
       (check-equal? ((λ x x) 1 2 3) '(1 2 3))
       (check-equal? ((λ #:∀ (A) [x : A ...*] : (Listof A) x) 1 2 3) '(1 2 3))]

@subsection{@racket[struct]}

@chunk[<struct>
       (define-syntax (new-struct stx)
         (syntax-parse stx
           [(_ tvars:new-maybe-type-vars
               (~and name+parent (~or name:id [name:id parent:id]))
               ([field:id :colon type:expr] ...)
               . rest)
            (template (struct (?? tvars.maybe) name+parent
                        ([field : (template-expand-type tvars.vars type)] ...)
                        . rest))]))]

@chunk[<test-struct>
       (struct s0 ())
       (struct s1 ([x : Number]))
       (struct s2 ([x : Number] [y : Number]))
       (struct s3 ([x : Number] [y : Number]) #:transparent)
       (struct s4 () #:transparent)
       (struct (A B) s5 ([x : A] [y : B]) #:transparent)
       (struct (A B) s6 () #:transparent)
       
       (check (λ (a b) (not (equal? a b))) (s0) (s0))
       (check-equal? (s1-x (s1 123)) 123)
       (check-equal? (s2-x (s2 2 3)) 2)
       (check-equal? (s2-y (s2 2 3)) 3)
       (check-equal? (s3-x (s3 4 5)) 4)
       (check-equal? (s3-y (s3 4 5)) 5)
       (check-equal? (s4) (s4))
       (check-equal? (s5-x (s5 6 7)) 6)
       (check-equal? (s5-y (s5 6 7)) 7)
       (check-equal? (s5 6 7) (s5 6 7))
       (check-equal? (s6) (s6))]

@subsection{@racket[ann]}

@chunk[<ann>
       (define-syntax/parse (new-ann value:expr (~optional :colon) type:expr)
         (template (ann value (template-expand-type () type))))]

@chunk[<test-ann>
       (let ()
         (define-type-expander (Repeat stx)
           (syntax-case stx ()
             [(_ t n) #`(List #,@(map (λ (x) #'t)
                                      (range (syntax->datum #'n))))]))
         (check-equal? (ann (ann '(1 2 3)
                                 (Repeat Number 3))
                            (List Number Number Number))
                       '(1 2 3)))]

@subsection{@racket[inst]}

@chunk[<inst>
       (define-syntax/parse (new-inst v
                                      (~optional :colon) t ...
                                      (~optional (~seq last (~datum ...) b:id)))
         (template (inst v (template-expand-type () t) ...
                         (?? (?@ (template-expand-type () last)
                                 (... ...) b)))))]

@chunk[<test-inst>
       (let ()
         (define-type-expander (Repeat stx)
           (syntax-case stx ()
             [(_ t n) #`(List #,@(map (λ (x) #'t)
                                      (range (syntax->datum #'n))))]))
         
         (: f (∀ (A B C D) (→ (Pairof A B) (Pairof C D) (List A C B D))))
         (define (f x y) (list (car x) (car y) (cdr x) (cdr y)))
         
         (check-equal? ((inst f
                              (Repeat Number 3)
                              (Repeat String 2)
                              (Repeat 'x 1)
                              (Repeat undefined-type 0))
                        '((1 2 3) . ("a" "b"))
                        '((x) . ()))
                       '((1 2 3) (x) ("a" "b") ())))]

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
          (let (?? (?@ loop (?? (?@ : (template-expand-type tvars.vars
                                                            return-type)))))
            (?@ . tvars)
            ([(?@ . name.expanded) e] ...)
            . rest)))]

@chunk[<test-let>
       (check-equal? (let loop-id ([x 1])
                       (if (= x 2)
                           x
                           (loop-id (+ x 1))))
                     2)
       (check-equal? (let () 'x) 'x)
       (check-equal? (ann (let #:∀ (T) ([a : T 3]
                                        [b : (Pairof T T) '(5 . 7)])
                            (cons a b))
                          (Pairof Number (Pairof Number Number)))
                     '(3 5 . 7))]

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

@chunk[<test-let*>
       (let ()
         (define-type-expander (Repeat stx)
           (syntax-case stx ()
             [(_ t n) #`(List #,@(map (λ (x) #'t)
                                      (range (syntax->datum #'n))))]))
         
         (check-equal? (let* ([x* : (Repeat Number 3) '(1 2 3)]
                              [y* : (Repeat Number 3) x*])
                         y*)
                       '(1 2 3)))]

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

@chunk[<test-let-values>
       (let ()
         (define-type-expander (Repeat stx)
           (syntax-case stx ()
             [(_ t n) #`(List #,@(map (λ (x) #'t)
                                      (range (syntax->datum #'n))))]))
         
         (check-equal? (ann (let-values
                                ([([x : (Repeat Number 3)])
                                  (list 1 2 3)])
                              (cdr x))
                            (List Number Number))
                       '(2 3))
         
         (check-equal? (ann (let-values
                                ([([x : (Repeat Number 3)] [y : Number])
                                  (values (list 1 2 3) 4)])
                              (cons y x))
                            (Pairof Number (List Number Number Number)))
                       '(4 . (1 2 3)))
         
         (check-equal? (ann (let-values
                                ([(x y)
                                  (values (list 1 2 3) 4)])
                              (cons y x))
                            (Pairof Number (List Number Number Number)))
                       '(4 . (1 2 3))))]

@subsection{@racket[make-predicate]}

@chunk[<make-predicate>
       (define-syntax/parse (new-make-predicate type:expr)
         (template (make-predicate (template-expand-type () type))))]

@chunk[<test-make-predicate>
       (let ()
         (define-type-expander (Repeat stx)
           (syntax-case stx ()
             [(_ t n) #`(List #,@(map (λ (x) #'t)
                                      (range (syntax->datum #'n))))]))
         (check-equal? ((make-predicate (Repeat Number 3)) '(1 2 3)) #t)
         (check-equal? ((make-predicate (Repeat Number 3)) '(1 "b" 3)) #f))]

@subsection[#:tag "other-forms"]{Other @racket[typed/racket] forms}

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
        define-struct/exec)]

@section{Future work}

We have not implemented alternative type-expanding definitions for all the
@tc[typed/racket] forms, as noted in @secref{other-forms}.

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
                  "../lib/low.rkt")
         (require (for-syntax (submod ".." expander)))
         
         (provide new-:)
         
         <:>)]

Since our @tc[new-:] macro needs to call the @tc[type-expander], and the other
forms too, we can't define @tc[type-expander] in the same module as these forms,
it needs to be either in the same module as @tc[new-:], or in a separate module.
Additionally, expand-type needs to be required @tc[for-syntax] by the forms, but
needs to be @tc[provide]d too, so it is much easier if it is defined in a
separate module (that should be used only @tc[for-syntax], so it will be written
in @tc[racket], not @tc[typed/racket]).

@chunk[<module-expander>
       (module expander racket
         (require racket)
         (require syntax/parse)
         (require syntax/stx)
         
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
         <expand-type>)]

We can finally define the overloaded forms, as well as the extra
@tc[<define-type-expander>].

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax racket
                              racket/syntax
                              syntax/parse
                              syntax/parse/experimental/template)
                  "../lib/low.rkt")
         
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
                              [new-ann ann]
                              [new-inst inst]
                              [new-let let]
                              [new-let* let*]
                              [new-let-values let-values]
                              [new-make-predicate make-predicate]))
         
         <define-type-expander>
         
         (begin-for-syntax
           <template-metafunctions>
           <syntax-classes>)
         
         <define-type>
         <define>
         <lambda>
         <struct>
         <ann>
         <inst>
         <let>
         <let*>
         <let-values>
         <make-predicate>
         <other-forms>)]

And, last but not least, we will add a @tc[test] module.

@chunk[<module-test>
       (module* test typed/racket
         (require typed/rackunit)
         (require (submod ".."))
         (require (for-syntax (submod ".." expander)))
         (require (for-syntax racket/list))
         
         <test-expand-type>
         
         <test-:>
         <test-define-type>
         <test-define>
         <test-lambda>
         <test-struct>
         <test-ann>
         <test-inst>
         <test-let>
         <test-let*>
         <test-let-values>
         <test-make-predicate>
         
         ;; Make the code coverage take the docs into account.
         (require (submod ".." doc)))]

We can now assemble the modules in this order:

@chunk[<*>
       (begin
         <module-expander>
         <module-colon>
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]
