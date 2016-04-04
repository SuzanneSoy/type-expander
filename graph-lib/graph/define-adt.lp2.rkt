#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: 
 @racket[define-constructor] and @racket[define-tagged]}

@(table-of-contents)

@section{Introduction}

@section{@racket[uninterned] and @racket[private]}

We wish to be able to declare tags and constructors only
visible to the creator, unlike the default ones which can be
instantiated and matched against anonymously.

We will define two flavours. In the first case, 
@tc[uninterned] constructors inherit the interned one. It
means that the interned constructor is a supertype of the
uninterned constructor (but not the converse). Two distinct
uninterned constructors with the same name are unrelated
too. The second possibility is to declare a @tc[private]
constructor, where the private constructor inherits directly
from @tc[ConstructorTop], the base structure described in
section @secref{constructor|supertype}, and is therefore
unrelated to the interned constructor (and is unrelated to
the uninterned constructor too).

The choice to declare an uninterned or private 

@CHUNK[<uninterned+private>
       (define/syntax-parse ((~maybe with-struct declare-uninterned/private))
         (cond [(attribute uninterned)
                #`(#,(syntax-local-introduce #'constructor-name)
                   declare-uninterned-constructor-struct)]
               [(attribute private)
                #`(#,(syntax-local-introduce #'constructor-name)
                   declare-constructor-struct)]
               [else #'()]))]

The above code binds @tc[declare-uninterned/private] to
either @tc[declare-uninterned-constructor-struct] or 
@tc[declare-constructor-struct], depending on the keyword
used. The macro's expansion will use this to declare 
@tc[with-struct].

@chunk[<declare-uninterned-or-private-struct>
       (?? (declare-uninterned/private with-struct))]

@section{@racket{define-constructor}}

@chunk[<define-constructor>
       (define-syntax/parse
           (define-constructor constructor-name:id
             (~maybe (~optkw #:uninterned) (~optkw #:private))
             (~maybe #:? name?)
             type …)
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define-temp-ids "pat" (type …))
         (define-temp-ids "value" (type …))
         <uninterned+private>
         (template
          (begin
            <declare-uninterned-or-private-struct>
            (define-multi-id constructor-name
              #:type-expand-once
              (constructor constructor-name
                           (?? (?@ #:with-struct with-struct))
                           type …)
              #:match-expander
              (λ/syntax-parse (_ pat …)
                #'(constructor constructor-name
                               (?? (?@ #:with-struct with-struct))
                               pat …))
              #:call
              (λ/syntax-parse (_ value …)
                #'(constructor constructor-name
                               (?? (?@ #:with-struct with-struct))
                               value …)))
            (define-multi-id (?? name? default-name?)
              #:else
              #'(constructor? constructor-name
                              (?? (?@ #:with-struct with-struct)))))))]

@chunk[<define-tagged>
       (define-syntax/parse (define-tagged constructor-name:id
                              (~maybe (~optkw #:uninterned) (~optkw #:private))
                              (~maybe #:? name?)
                              [field type] …)
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define-temp-ids "pat" (type …))
         (define-temp-ids "value" (type …))
         <uninterned+private>
         (template
          (begin
            <declare-uninterned-or-private-struct>
            (define-multi-id constructor-name
              #:type-expand-once
              (tagged constructor-name
                      (?? (?@ #:with-struct with-struct))
                      [field type] …)
              #:match-expander
              (λ/syntax-parse (_ pat …)
                #'(tagged constructor-name
                          (?? (?@ #:with-struct with-struct))
                          [field pat] …))
              #:call
              (λ/syntax-parse (_ value …)
                #'(tagged #:instance
                          constructor-name
                          (?? (?@ #:with-struct with-struct))
                          [field value] …)))
            (define-multi-id (?? name? default-name?)
              #:else
              #'(tagged? constructor-name
                         (?? (?@ #:with-struct with-struct))
                         field …)))))]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket/list
                                syntax/parse
                                syntax/parse/experimental/template
                                racket/syntax
                                (submod "../lib/low.rkt" untyped))
                    (for-meta 2 racket/base)
                    "constructor.lp2.rkt"
                    (submod "constructor.lp2.rkt" main private)
                    "tagged.lp2.rkt"
                    "../lib/low.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           (provide define-constructor
                    define-tagged)
           
           <define-constructor>
           <define-tagged>)
         
         (require 'main)
         (provide (all-from-out 'main)))]