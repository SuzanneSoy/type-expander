#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: @racket[uniform-get]}

@(table-of-contents)

@section{Introduction}

@section{Wrapped structures}

@chunk[<wrapped-structure>
       ;; Pre-declare the tag.
       (let ()
         (tagged #:instance wstructure)
         (void))
       
       (define-multi-id wrapped-structure
         #:type-expander
         (λ/syntax-parse (_ . rest)
           #'(tagged wstructure (structure . rest)))
         #:match-expander
         (λ/syntax-parse (_ . rest)
           #'(tagged wstructure (structure . rest)))
         #:call
         (λ/syntax-parse (_ . rest)
           #'(tagged wstructure (structure . rest))))

       (define-type-expander (wrapped-structure-supertype stx)
         (syntax-case stx ()
           [(_ . rest)
            #'(constructor wstructure
                           (Promise
                            (structure-supertype . rest)))]))]

@section{@racket[uniform-get]}

@racket[uniform-get] operates on tagged structures. It
retrieves the desired field from the structure.

@chunk[<uniform-get>
       (define-syntax-rule (uniform-get v field)
         (structure-get (force (constructor-values v)) field))]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket/list
                                syntax/parse
                                syntax/parse/experimental/template
                                racket/syntax
                                (submod phc-toolkit untyped))
                    phc-toolkit
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "constructor.lp2.rkt"
                    "tagged.lp2.rkt"
                    "structure.lp2.rkt"
                    "define-adt.lp2.rkt")
           
           (provide wrapped-structure
                    wrapped-structure-supertype
                    uniform-get)
           
           <wrapped-structure>
           <uniform-get>)
         
         (require 'main)
         (provide (all-from-out 'main)))]