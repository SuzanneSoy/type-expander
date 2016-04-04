#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: Constructor}

@(table-of-contents)

@section{Introduction}

This file defines @tc[constructor], a form which allows
tagging values, so that two otherwise identical values can
be distinguished by the constructors used to wrapp them.

@section[#:tag "constructor|supertype"]{The @racket[ConstructorTop] supertype}

We define variants as instances of subtypes of the @tc[Tagged] structure:

@chunk[<constructor-top>
       (struct (T) ConstructorTop ([values : T]) #:transparent)
       (define-type ConstructorTopType ConstructorTop)]

Other options would include defining the variant as a @tc[list], with the tag
symbol in the first element. We couldn't use a @tc[vector], because these are
mutable in @tc[typed/racket] (for now), and occurrence typing can't work
properly on mutable data structures. Using a @tc[list] has the drawback that
other data can easily take the same shape, meaning that it is impossible to
define a reliable predicate for a tagged instance that also works well with*
@tc[typed/racket]'s occurrence typing.

@section{Declaring a new constructor}

The constructor type will be a @tc[struct] inheriting from 
@tc[ConstructorTop], without adding any field. By default, the
constructor's name is "interned" (not in racket's interned
symbols sense), so that two uses of the same constructor
name in different files refer to the same constructor type.

For this, we use the @tc[remember] library:

@chunk[<remember-lib>
       (require (for-syntax "remember-lib.rkt"))]

We pre-declare here in this file all the remembered constructors:

@CHUNK[<declare-constructor-struct>
       (define-syntax (declare-constructor-struct stx)
         (syntax-case stx ()
           [(_ name)
            #`(struct (T)
                name
                #,(syntax-local-introduce #'ConstructorTop)
                ()
                #:transparent)]))]

@CHUNK[<declare-uninterned-constructor-struct>
       (define-syntax (declare-uninterned-constructor-struct stx)
         (syntax-parse stx
           [(_ name)
            (define/syntax-parse ((~maybe no-with-struct)) #'())
            (with-constructor-name→stx-name
                (parent no-with-struct #'name #'please-recompile stx)
              #'(struct (T)
                  name
                  parent
                  ()
                  #:transparent))]))]

@CHUNK[<constructor-declarations>
       (declare-constructor-struct constructor-name/struct)
       …]

We define an associative list which maps the constructor
name to the structure identifier (with the same scopes as
the one declared just above):

@CHUNK[<declare-all-constructors>
       (define-syntax (declare-all-constructors stx)
         (define/with-syntax (constructor-name …)
           constructor-names-no-duplicates)
         (define/with-syntax alist
           (syntax-local-introduce #'constructor-name→stx-name/alist))
         (define-temp-ids "~a/struct" (constructor-name …))
         #`(begin
             <constructor-declarations>
             
             (define-for-syntax alist
               (stx-map (λ (x y) (cons (syntax->datum x) y))
                        #'(constructor-name …)
                        #'(constructor-name/struct …)))))]

We call this macro once, to define the structs in the
template meta-level, and the 
@tc[constructor-name→stx-name/alist] in the transformer
meta-level.

@CHUNK[<declare-all-constructors>
       (declare-all-constructors)]

The list of constructor names, @tc[constructor-names-no-duplicates], is the one
remembered by “@code{remember-lib.rkt}” with duplicate entries removed:

@CHUNK[<named-sorted-constructors>
       (define-for-syntax constructor-names-no-duplicates
         (remove-duplicates (get-remembered 'constructor)))]

Finally, we define @tc[with-constructor-name→stx-name], a
helper macro which accesses the structure identifier for a
given constructor name, checking whether the constructor
name has been remembered already (and throwing an error
otherwise):

@chunk[<with-constructor-name→stx-name>
       (begin-for-syntax
         (define-syntax (with-constructor-name→stx-name stx)
           (syntax-case stx ()
             [(_ (stx-name with-struct constructor-name fallback error-stx)
                 . body)
              #`<with-constructor-name→stx-name-body>])))]

@chunk[<with-constructor-name→stx-name-body>
       (if #,(syntax/loc #'with-struct (attribute with-struct))
           (with-syntax ([stx-name #'with-struct])
             . body)
           (if (check-remember-all 'constructor constructor-name)
               (with-syntax
                   ([stx-name (cdr
                               (assoc (syntax->datum
                                       (datum->syntax #f constructor-name))
                                      constructor-name→stx-name/alist))])
                 . body)
               ;; TODO: set srcloc of fallback to stx on the next line:
               (remember-all-errors2 fallback constructor-name)))]

@section{@racket[constructor]}

We define the @tc[constructor] macro which acts as a type, a
match expander, and a procedure returning a constructor
instance:

@chunk[<constructor>
       (define-multi-id constructor
         #:type-expander <type-expander>
         #:match-expander <match-expander>
         #:call <make-instance>)]

@subsection{Type-expander}

@CHUNK[<type-expander>
       (λ/syntax-parse (_ constructor-name:id
                          (~maybe #:with-struct with-struct)
                          . (~or (T₀:expr) (Tᵢ:expr …)))
         (with-constructor-name→stx-name
             (stx-name with-struct #'constructor-name #'please-recompile stx)
           (template
            (stx-name (?? T₀ (List Tᵢ …))))))]

@subsection{Predicate}

@CHUNK[<predicate>
       (define-syntax (Constructor-predicate? stx)
         (syntax-parse stx
           [(_ constructor-name (~maybe #:with-struct with-struct) v)
            (quasisyntax/loc stx
              (#,(template/loc stx
                   (Constructor-predicate? constructor-name
                                           (?? (?@ #:with-struct with-struct))))
               v))]
           [(_ constructor-name (~maybe #:with-struct with-struct))
            ;; make-predicate works for polymorphic structs when
            ;; instantiating them with Any.
            (with-constructor-name→stx-name
                (stx-name with-struct
                          #'constructor-name
                          (syntax/loc #'constructor-name please-recompile)
                          stx)
              (syntax/loc stx (make-predicate (stx-name Any))))]
           [(_)
            (syntax/loc stx (make-predicate (ConstructorTop Any)))]))]

@subsection{Match-expander}

@chunk[<match-expander>
       (λ/syntax-parse (_ constructor-name:id (~maybe #:with-struct with-struct)
                          . (~or (pat₀:expr) (patᵢ:expr …)))
         (template
          (? (Constructor-predicate? constructor-name
                                     (?? (?@ #:with-struct with-struct)))
             (app ConstructorTop-values (?? pat₀ (list patᵢ …))))))]

@subsection{Instance creation}

@CHUNK[<make-instance>
       (λ/syntax-parse (_ constructor-name:id
                          (~maybe #:with-struct with-struct)
                          value:expr …)
         (define/with-syntax (arg …) (generate-temporaries #'(value …)))
         (define/syntax-parse (~or (arg₀) (argᵢ …)) #'(arg …))
         (define/with-syntax (T …) (generate-temporaries #'(value …)))
         (with-constructor-name→stx-name
             (stx-name with-struct
                       #'constructor-name
                       (syntax/loc #'constructor-name please-recompile)
                       stx)
           (template
            ((λ #:∀ (T …) ([arg : T] …)
               : (constructor constructor-name
                              (?? (?@ #:with-struct with-struct))
                              T …)
               (stx-name (?? arg₀ (list argᵢ …))))
             value …))))]

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket/list
                                syntax/parse
                                syntax/parse/experimental/template
                                racket/syntax
                                (submod "../lib/low.rkt" untyped))
                    (for-meta 2 racket/base)
                    "../lib/low.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           (provide constructor
                    (rename-out [Constructor-predicate? constructor?])
                    (rename-out [ConstructorTopType ConstructorTop])
                    ConstructorTop?
                    (rename-out [ConstructorTop-values constructor-values]))
           
           <constructor-top>
           <declare-constructor-struct>
           <remember-lib>
           <named-sorted-constructors>
           <declare-all-constructors>
           <with-constructor-name→stx-name>
           <declare-uninterned-constructor-struct>
           
           <constructor>
           <predicate>
           
           (module+ private
             (provide declare-constructor-struct)))
         
         (require 'main)
         (provide (all-from-out 'main)))]