#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Rewriting data structures and their types}

This module allows purely functional substitution inside a data structure of
arbitrarily deep elements of a given type, while also computing the type of the
result.

For example, one could replace all strings in a data structure by their length:

@chunk[<test-example>
       'a
       (begin-for-syntax 
         (displayln
          (syntax->datum
           (replace-in-data-structure #'(List (Pairof Symbol String))
                                      #'([String Number string-length])))))
       #;(define-syntax (string→number stx)
           (replace-in-data-structure
            #'(List (Pairof Symbol String))
            #'[String Number string-length]))]

@CHUNK[<replace-in-data-structure>
       (define-for-syntax (replace-in-data-structure t r)
         (define/with-syntax ([from to fun] ...) r)
         (syntax-parse t
           [x:id
            #:attr assoc-from-to (stx-assoc #'x #'((from . to) ...))
            #:when (attribute assoc-from-to)
            #'assoc-from-to]
           [((~literal List) a ...)
            #`(List #,@(stx-map (λ (x) (replace-in-data-structure x r))
                                #'(a ...)))]
           [((~literal Pairof) a b)
            #`(Pairof #,(replace-in-data-structure #'a r)
                      #,(replace-in-data-structure #'b r))]
           [x:id #'x]))]

@chunk[<*>
       (begin
         (module main typed/racket;;;;;;;;;;
           (require (for-syntax syntax/parse
                                racket/syntax
                                syntax/stx
                                "../lib/low-untyped.rkt")
                    "structure.lp2.rkt"
                    "variant.lp2.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           (begin-for-syntax (provide replace-in-data-structure))
           
           <replace-in-data-structure>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    typed/rackunit
                    "structure.lp2.rkt"
                    "variant.lp2.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           <test-example>
           
           (require (submod ".." doc))))]