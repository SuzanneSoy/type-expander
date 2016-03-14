#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Variants}

@(table-of-contents)

@section{Introduction}

We define variants (tagged unions), with the following constraints:

@itemlist[
 @item{Unions are anonymous: two different unions can contain the same tag, and
  there's no way to distinguish these two occurrences of the tag}
 @item{Callers can require an uninterned tag which inherits the interned tag, so
  that @racket[(tagged #:uninterned tag Number)] is a subtype of
  @racket[(tagged #:uninterned tag Number)], but not the reverse}
 @item{The tag can be followed by zero or more “fields”}
 @item{An instance of a variant only @racket[match]es with its constructor and
  the same number of fields, with exact matching for uninterned tags}]

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

@section[#:tag "variant|supertype"]{The @racket[Variant] supertype}

We define variants as instances of subtypes of the @tc[Tagged] structure:

@chunk[<variant-supertype>
       (struct (T) Tagged ([value : T]) #:transparent)
       (define-type Tagged-type Tagged)]

Other options would include defining the variant as a @tc[list], with the tag
symbol in the first element. We couldn't use a @tc[vector], because these are
mutable in @tc[typed/racket] (for now), and occurrence typing can't work
properly on mutable data structures. Using a @tc[list] has the drawback that
other data can easily take the same shape, meaning that it is impossible to
define a reliable predicate for a tagged instance that is also understood by
@tc[typed/racket].

@section{Declaring a new tag}

The tag type will be a @tc[struct] inheriting from 
@tc[Tagged], without adding any field. By default, the tag
is "interned" (not in racket's interned symbols sense), so
that two uses of the same tag name in different files refer
to the same tag type.

For this, we use the @tc[remember] library:

@chunk[<remember-tags>
       (require (for-syntax "remember-lib.rkt"))]

We pre-declare here in this file all the remembered tags:

@CHUNK[<tag-declarations>
       (struct (T) tag-name/struct Tagged () #:transparent)
       …]

We define an associative list which maps tag names to the
structure identifier (with the same scopes as the one
declared just above):

@CHUNK[<declare-all-tags>
       (define-syntax/parse (declare-all-tags tag-name→stx-name/alist:id
                                              tag-name …)
         (define-temp-ids "~a/struct" (tag-name …))
         #'(begin
             <tag-declarations>
             
             (define-for-syntax tag-name→stx-name/alist
               (stx-map (λ (x y) (cons (syntax->datum x) y))
                        #'(tag-name …)
                        #'(tag-name/struct …)))))]

This macro should be called only once, and given as parameters the whole
remembered list of tag names:

@CHUNK[<declare-all-tags>
       (define-syntax/parse (call-declare-all-tags tag-name→stx-name/alist:id)
         #`(declare-all-tags tag-name→stx-name/alist
                             #,@tag-names-no-duplicates))
       
       (call-declare-all-tags tag-name→stx-name/alist)]

This list of tag names is the one remembered by
“@code{remember-lib.rkt}” with duplicate entries removed:

@CHUNK[<named-sorted-tags>
       (define-for-syntax tag-names-no-duplicates
         (remove-duplicates (get-remembered 'variant)))]

Finally, we define @tc[with-tag-name→stx-name], a helper
macro which accesses the structure identifier for a given
tag name, checking whether the tag name has been remembered
already:

@chunk[<tag-name→stx-name>
       (begin-for-syntax
         (define-syntax-rule (with-tag-name→stx-name
                                 (stx-name tag-name fallback error-stx)
                               . body)
           (if (check-remember-all 'variant tag-name)
               (with-syntax ([stx-name (cdr (assoc (syntax->datum
                                                    (datum->syntax #f tag-name))
                                                   tag-name→stx-name/alist))])
                 . body)
               (remember-all-errors2 fallback tag-name))))]

@section{@racket[constructor]}

We define the @tc[constructor] macro which acts as a type,
a match expander, and a procedure returning a tagged
instance:

@chunk[<constructor>
       (define-multi-id constructor
         #:type-expander <type-expander>
         #:match-expander <match-expander>
         #:call <make-instance>)]

@chunk[<test-constructor>
       (check-equal?: (Tagged-value
                       (ann (constructor a 1 "x")
                            (constructor a Number String)))
                      (list 1 "x")) ;; TODO: test that the tag is 'a
       (check-equal?: (Tagged-value
                       (ann (constructor b)
                            (constructor b)))
                      (list)) ;; TODO: test that the tag is 'b
       (check-equal?: (Tagged-value
                       (ann (constructor c 'd)
                            (constructor c Symbol)))
                      'd) ;; TODO: test that the tag is 'c
       (check-equal?: (ann (constructor c 2 "y")
                           (constructor c Number String))
                      (constructor c 2 "y"))
       (check-not-equal?: (constructor d 2 "y")
                          (constructor d 2 "y" 'z))
       (check-not-equal?: (constructor e 2 "y")
                          (constructor F 2 "y"))]

@subsection{Type-expander}

@CHUNK[<type-expander>
       (λ/syntax-parse (_ tag:id . (~or (T₀:expr) (Tᵢ:expr …)))
         (with-tag-name→stx-name (stx-name #'tag #'please-recompile stx)
           (quasitemplate
            (stx-name (?? T₀ (List Tᵢ …))))))]

@subsection{Predicate}

@CHUNK[<predicate>
       (define-syntax (Tagged-predicate? stx)
         (syntax-case stx ()
           [(_ tag v)
            #'((Tagged-predicate? tag) v)]
           [(_ tag)
            ;; make-predicate works for polymorphic structs when
            ;; instantiating them with Any.
            (with-tag-name→stx-name
                (stx-name #'tag (syntax/loc #'tag please-recompile) stx)
              #`(make-predicate (stx-name Any)))]
           [(_)
            #'(make-predicate (Tagged Any))]))]

@subsection{Match-expander}

@chunk[<match-expander>
       (λ/syntax-parse (_ tag:id . (~or (pat₀:expr) (patᵢ:expr …)))
         (template
          (and (? (Tagged-predicate? tag))
               (app Tagged-value (?? pat₀ (list patᵢ …))))))]

@subsection{Actual constructor}

@CHUNK[<make-instance>
       (λ/syntax-parse (_ tag:id value:expr …)
         (define/with-syntax (arg …) (generate-temporaries #'(value …)))
         (define/syntax-parse (~or (arg₀) (argᵢ …)) #'(arg …))
         (define/with-syntax (T …) (generate-temporaries #'(value …)))
         (with-tag-name→stx-name
             (stx-name #'tag (syntax/loc #'tag please-recompile) stx)
           (template
            ((λ #:∀ (T …) ([arg : T] …)
               : (constructor tag T …)
               (stx-name (?? arg₀ (list argᵢ …))))
             value …))))]

@section{@racket[define-variant]}

In @tc[define-variant], we only define the type (which is the union of all the
possible constructors. We don't define the constructors, for two reasons: the
same @tc[constructor]s could appear in several variants, so we would define them
twice, and it is likely that a constructor will have the same identifier as an
existing variable or function.

@CHUNK[<define-variant>
       (define-syntax/parse (define-variant name [tag:id type:expr …] …
                              (~maybe #:? name?))
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define-temp-ids "pat" ((type …) …))
         (if (andmap (λ (t) (check-remember-all 'variant t))
                     (syntax->list #'(tag …)))
             (let ()
               (define/with-syntax (stx-name …)
                 (stx-map (λ (t)
                            (cdr (assoc (syntax->datum (datum->syntax #f t))
                                        tag-name→stx-name/alist)))
                          #'(tag …)))
               (quasitemplate
                (begin
                  (define-type name (U (constructor tag type …) …))
                  (: (?? name? default-name?)
                     (→ Any Boolean :
                        #:+ (or (stx-name Any) …)
                        #:- (and (! (stx-name Any)) …)))
                  (define ((?? name? default-name?) x)
                    (or (Tagged-predicate? tag x) …)))))
             (stx-map (λ (t)
                        (remember-all-errors2 (syntax/loc t #'please-recompile)
                                              t))
                      #'(tag …))))]

@chunk[<test-define-variant>
       (define-variant v1 [x Number String] [y String Number] [z Number String])
       (check-equal?: (ann (constructor x 1 "a")
                           (U [constructor w Number String]
                              [constructor x Number String]
                              [constructor y String Number]))
                      (constructor x 1 "a"))
       (check-equal?: (constructor x 1 "a")
                      (constructor x 1 "a"))
       (check-equal?: (ann (constructor x 1 "a") v1)
                      (constructor x 1 "a"))
       (check-equal?: (ann (constructor x 1 "a") v1)
                      (ann (constructor x 1 "a") v1))
       (check-not-equal?: (ann (constructor x 2 "b") v1)
                          (ann (constructor y "b" 2) v1))
       (check-not-equal?: (ann (constructor x 3 "c") v1)
                          (ann (constructor z 3 "c") v1))]

This makes pattern-matching more verbose, though, since we have to specify
@tc[(variant tag pat …)] each time, instead of just @tc[(tag pat …)]. I
don't really know how to solve that. It should be noted that constructors are
likely to have names starting with a capital letter, so maybe this reduces the
number of name collisions.

@section{@racket[tagged]}

@CHUNK[<tagged>
       (define-multi-id tagged
         #:type-expander
         (λ/syntax-parse (_ tag:id . structure-type)
           #`(constructor tag #,(syntax/loc #'structure-type
                                  (structure . structure-type))))
         #:match-expander
         (λ/syntax-parse (_ tag:id . structure-pat)
           #`(constructor tag #,(syntax/loc #'structure-pat
                                  (structure . structure-pat))))
         #:call ;; TODO: clean this up a bit, and explain it.
         (λ/syntax-parse
             (~and (_ (~and (~seq disambiguate …) (~or (~seq #:instance)
                                                       (~seq #:constructor)
                                                       (~seq)))
                      tag:id . fields)
                   (~parse (sa:structure-args-stx-class)
                           #'(disambiguate … . fields)))
           (define-temp-ids "~a/TTemp" (sa.field …))
           (define-temp-ids "~a/arg" (sa.field …))
           (define/with-syntax c
             (if (attribute sa.type)
                 #`(λ ([sa.field/arg : sa.type] …)
                     : (constructor tag #,(syntax/loc #'fields
                                            (structure [sa.field sa.type] …)))
                     (constructor tag
                                  #,(syntax/loc #'fields
                                      (structure #:instance
                                                 [sa.field : sa.type
                                                  sa.field/arg]
                                                 …))))
                 #`(λ #:∀ (sa.field/TTemp …) ([sa.field/arg : sa.field/TTemp] …)
                     : (constructor tag #,(syntax/loc #'fields
                                            (structure [sa.field sa.field/TTemp]
                                                       …)))
                     (constructor tag
                                  #,(syntax/loc #'fields
                                      (structure #:instance
                                                 [sa.field sa.field/arg] …))))))
           (if (attribute sa.value)
               #'(c sa.value …)
               #'c)))]

@CHUNK[<tagged>
       (define-multi-id any-tagged
         #:type-expander
         (λ/syntax-parse (_ . structure-type)
           #'(Tagged (structure . structure-type)))
         ;; This would require each tag struct to contain a field with its
         ;; tag name. We'll implement it if we need that kind of reflection.
         #|
         #:match-expander
         (λ/syntax-parse (_ tag-pat:id . structure-pat)
           #`(any-constructor (? symbol? tag-pat:id)
                              #,(syntax/loc #'structure-pat
                                  (structure . structure-pat))))|#)]

@chunk[<test-tagged>
       (check-equal?: (match (ann (tagged foo [x "o"] [y 3] [z 'z])
                                  (tagged foo
                                          [x String]
                                          [z 'z]
                                          [y Fixnum]))
                        [(tagged foo z x y) (list z y x)])
                      '(z 3 "o"))]

@section{@racket[define-tagged]}

@chunk[<define-tagged>
       (define-syntax/parse (define-tagged tag:id
                              (~maybe #:? tag?)
                              [field type] …)
         (define/with-syntax (pat …) (generate-temporaries #'(field …)))
         (define/with-syntax (value …) (generate-temporaries #'(field …)))
         (define/with-syntax default-tag? (format-id #'tag "~a?" #'tag))
         (template
          (begin
            (define-multi-id tag
              #:type-expand-once
              (tagged tag [field type] …)
              #:match-expander
              (λ/syntax-parse (_ pat …)
                #'(tagged tag [field pat] …))
              #:call
              (λ/syntax-parse (_ value …)
                #'(tagged tag #:instance [field value] …)))
            (: (?? tag? default-tag?) (→ Any Boolean))
            (define ((?? tag? default-tag?) x)
              (and (Tagged-predicate? tag x)
                   ((structure? field …) (Tagged-value x)))))))]

@chunk[<test-define-tagged>
       (define-tagged tagged-s1)
       (define-tagged tagged-s2 [f Fixnum] [g String])
       (define-tagged tagged-s3 [g String] [f Fixnum])
       (define-tagged tagged-s4 [f Fixnum] [g String])
       
       (check-equal?: (match (ann (tagged-s1) (tagged tagged-s1))
                        [(tagged-s1) #t])
                      #t)
       
       (check-equal?: (match (ann (tagged-s2 99 "z") tagged-s2)
                        [(tagged-s2 f g) (cons g f)])
                      '("z" . 99))
       
       (let ()
         (check-equal?: (match (ann (tagged-s2 99 "in-let") tagged-s2)
                          [(tagged-s2 f g) (cons g f)])
                        '("in-let" . 99)))
       
       (define (test-match val)
         (match val
           [(tagged-s2 x y) (list 'found-s2 y x)]
           [(tagged-s3 x y) (list 'found-s3 y x)]
           [(tagged-s4 x y) (list 'found-s4 y x)]))
       
       (check-equal?:
        (test-match (ann (tagged-s2 2 "flob")
                         (tagged tagged-s2 [f Fixnum] [g String])))
        '(found-s2 "flob" 2))
       
       (check-equal?:
        (test-match (ann (tagged-s3 "flob" 2)
                         (tagged tagged-s3 [g String] [f Fixnum])))
        '(found-s3 2 "flob"))
       
       ;; g and f are inverted in the “ann”
       (check-equal?:
        (test-match (ann (tagged-s4 2 "flob")
                         (tagged tagged-s4 [g String] [f Fixnum])))
        '(found-s4 "flob" 2))
       
       (define (test-match-verbose val)
         (match val
           [(tagged tagged-s2 g [f y]) (list 'found-s2 g y)]
           [(tagged tagged-s3 [g y] f) (list 'found-s2 f y)]
           [(tagged tagged-s4 [f y] g) (list 'found-s2 g y)]))
       
       (check-equal?:
        (test-match (ann (tagged-s2 3 "flob")
                         (tagged tagged-s2 [f Fixnum] [g String])))
        '(found-s2 "flob" 3))
       
       ;; g and f are inverted in the “ann”
       (check-equal?:
        (test-match (ann (tagged-s3 "flob" 3)
                         (tagged tagged-s3 [f Fixnum] [g String])))
        '(found-s3 3 "flob"))
       
       (check-equal?:
        (test-match (ann (tagged-s4 3 "flob")
                         (tagged tagged-s4 [f Fixnum] [g String])))
        '(found-s4 "flob" 3))
       
       (check-not-equal?: (tagged-s2 4 "flob")
                          (tagged-s3 "flob" 4))
       (check-not-equal?: (tagged-s2 4 "flob")
                          (tagged-s4 4 "flob"))]

@section{Uninterned tags}

We wish to be able to declare tags only visible to the
creator, unlike the ones above which are visible
everywhere.

We will define two flavours: one where uninterned tags
inherit the interned tag, os that the interned @tc[tag] is a
supertype of the uninterned @tc[tag] (but not the converse),
and a second, which we will call private tags, where the
private tag inherits directly from @tc[Tagged], the base
structure described in section @secref{variant|supertype},
and is therefore unrelated to the interned @tc[tag] (nor to
the uninterned @tc[tag] either).

@; TODO: this should be integrated a bit better with other function, for example
@; Tagged-predicate? (provided as Tagged?) is likely to not work on uninterned
@; tags.

@chunk[<define-uninterned-tagged>
       (define-syntax/parse
           (define-private-tagged tag:id
             (~maybe #:? tag?)
             . (~and structure-type
                     ([field (~optional (~and C :colon)) type] …)))
         (define/with-syntax default-tag? (format-id #'tag "~a?" #'tag))
         (define-temp-ids "~a/struct" tag)
         (define-temp-ids "~a/arg" (field …))
         (define-temp-ids "~a/pat" (field …))
         (template
          (begin
            (struct (T) tag/struct Tagged ()) ; Private
            ;(struct (T) tag/struct interned ()) ; Uninterned
            (define-multi-id tag
              #:type-expand-once
              (tag/struct (structure . structure-type))
              #:match-expander
              (λ/syntax-parse (_ . (~and structure-pat
                                         ((~and field/pat :expr) …)))
                (quasitemplate
                 (and (? (make-predicate (tag/struct Any)))
                      (app Tagged-value
                           #,(syntax/loc #'structure-pat
                               (structure [field field/pat] …))))))
              #:call
              (λ/syntax-parse (_ . (~and args ((~and field/arg :expr) …)))
                (quasitemplate
                 (tag/struct #,(syntax/loc #'args
                                 (structure #:instance
                                            [field : type field/arg] …))))))
            ;; TODO: the return type is not precise enough, it should be:
            ;; #:+ (tag/struct (structure Any …))
            ;; #:- (! (tag/struct (structure Any …)))
            (: (?? tag? default-tag?) (→ Any Boolean :
                                         #:+ (tag/struct Any)))
            (define ((?? tag? default-tag?) x)
              (and ((make-predicate (tag/struct Any)) x)
                   ((structure? field …) (Tagged-value x)))))))]

@chunk[<define-uninterned-constructor>
       (define-syntax/parse
           (define-private-constructor tag:id
             (~maybe #:? tag?)
             T:expr …)
         (define/with-syntax default-tag? (format-id #'tag "~a?" #'tag))
         (define-temp-ids "~a/struct" tag)
         (define-temp-ids "~a/arg" (T …))
         (define-temp-ids "~a/pat" (T …))
         (define/syntax-parse (~or ([T₀:expr arg₀ pat₀])
                                   ([Tᵢ:expr argᵢ patᵢ] …))
           #'([T T/arg T/pat] …))
         (template
          (begin
            (struct (X) tag/struct Tagged ()) ; Private
            ;(struct (X) tag/struct interned ()) ; Uninterned
            (define-multi-id tag
              #:type-expand-once
              (tag/struct (?? T₀ (List Tᵢ …)))
              #:match-expander
              (λ/syntax-parse (_ . (~and pats (?? ((~and pat₀ :expr))
                                                  ((~and patᵢ :expr) …))))
                (quasitemplate
                 (and (? (make-predicate (tag/struct Any)))
                      (app Tagged-value
                           #,(syntax/loc #'pats
                               (?? pat₀ (list patᵢ …)))))))
              #:call
              (λ/syntax-parse (_ . (~and args (?? ((~and arg₀ :expr))
                                                  ((~and argᵢ :expr) …))))
                (quasitemplate
                 (tag/struct #,(syntax/loc #'args
                                 (?? arg₀ (list argᵢ …)))))))
            (: (?? tag? default-tag?) (→ Any Boolean : (tag/struct Any)))
            (define ((?? tag? default-tag?) x)
              ((make-predicate (tag/struct Any)) x)))))]

@chunk[<test-uninterned-tagged>
       (define-syntax-rule (defp make mt)
         (begin
           (define-private-tagged txyz #:? txyz?
             [a Number]
             [b String])
           
           (define (make) (txyz 1 "b"))
           
           (define (mt v)
             (match v
               ((txyz x y) (list 'macro y x))
               (_ #f)))))
       
       (defp make mt)
       
       (define-private-tagged txyz #:? txyz?
         [a Number]
         [b String])
       
       (check-equal?: (match (make)
                        ((tagged txyz x y) (list 'out y x))
                        (_ #f))
                      #f)
       
       (check-equal?: (mt (tagged txyz [x 1] [y "b"]))
                      #f)
       
       (check-equal?: (mt (make))
                      '(macro "b" 1))
       
       (check-not-equal?: (make) (txyz 1 "b"))
       (check-equal?: (match (make)
                        ((txyz x y) (list 'out y x))
                        (_ #f))
                      #f)
       
       (check-equal?: (mt (txyz 1 "b"))
                      #f)]

@chunk[<test-uninterned-constructor>
       (define-syntax-rule (defpc makec mtc)
         (begin
           (define-private-constructor cxyz #:? cxyz? Number String)
           
           (define (makec) (cxyz 1 "b"))
           
           (define (mtc v)
             (match v
               ((cxyz x y) (list 'macro y x))
               (_ #f)))))
       
       (defpc makec mtc)
       
       (define-private-constructor cxyz #:? cxyz? Number String)
       
       (check-equal?: (match (makec)
                        ((constructor cxyz e f) (list 'out f e))
                        (_ #f))
                      #f)
       
       (check-equal?: (mtc (constructor cxyz 1 "b"))
                      #f)
       
       (check-equal?: (mtc (makec))
                      '(macro "b" 1))
       
       (check-not-equal?: (makec) (cxyz 1 "b"))
       (check-equal?: (match (makec)
                        ((cxyz e f) (list 'out f e))
                        (_ #f))
                      #f)
       
       (check-equal?: (mtc (cxyz 1 "b"))
                      #f)]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket/list
                                syntax/parse
                                syntax/parse/experimental/template
                                racket/syntax
                                (submod "../lib/low.rkt" untyped))
                    "../lib/low.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "structure.lp2.rkt")
           (provide (rename-out [Tagged-predicate? Tagged?]
                                [Tagged-type TaggedTop])
                    Tagged-value
                    constructor
                    define-variant
                    tagged
                    define-tagged
                    define-private-tagged
                    define-private-constructor
                    any-tagged)
           
           <variant-supertype>
           <remember-tags>
           <named-sorted-tags>
           <declare-all-tags>
           <tag-name→stx-name>
           
           <predicate>
           <constructor>
           <define-variant>
           <tagged>
           <define-tagged>
           <define-uninterned-tagged>
           <define-uninterned-constructor>
           
           (module+ test-helpers
             #;(provide Tagged-value)))
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    (submod ".." main test-helpers)
                    typed/rackunit
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           <test-constructor>
           <test-define-variant>
           <test-tagged>
           <test-define-tagged>
           <test-uninterned-tagged>
           <test-uninterned-constructor>))]
