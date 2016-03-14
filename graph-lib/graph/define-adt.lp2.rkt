#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: 
 @racket[define-constructor] and @racket[define-tagged]}

@(table-of-contents)

@section{Introduction}

@section{@racket{define-constructor}}

@chunk[<define-constructor>
       (define-syntax/parse
           (define-constructor constructor-name:id
             (~maybe #:with-struct with-struct)
             (~maybe #:? name?)
             type …)
         
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define-temp-ids "pat" (type …))
         (define-temp-ids "value" (type …))
         (template
          (begin
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
       (define-syntax/parse (define-tagged tag:id
                              (~maybe #:with-struct with-struct)
                              (~maybe #:? name?)
                              [field type] …)
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define-temp-ids "pat" (type …))
         (define-temp-ids "value" (type …))
         (template
          (begin
            (define-multi-id tag
              #:type-expand-once
              (tagged tag
                      (?? (?@ #:with-struct with-struct))
                      [field type] …)
              #:match-expander
              (λ/syntax-parse (_ pat …)
                #'(tagged tag
                          (?? (?@ #:with-struct with-struct))
                          [field pat] …))
              #:call
              (λ/syntax-parse (_ value …)
                #'(tagged #:instance
                          tag
                          (?? (?@ #:with-struct with-struct))
                          value …)))
            (define-multi-id (?? name? default-name?)
              #:else
              #'(tagged? tag
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
                    "../lib/low.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           <define-constructor>
           <define-tagged>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt")))]