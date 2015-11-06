#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Rewriting data structures and their types}

This module allows purely functional substitution inside a data structure of
arbitrarily deep elements of a given type, while also computing the type of the
result.

For example, one could replace all strings in a data structure by their length:

@CHUNK[<test-example>
       (begin-for-syntax 
         #;(displayln
          (syntax->datum
           (replace-in-instance #'(List (Pairof (Vector Symbol
                                                              (Vectorof String))
                                                      String))
                                      #'([String Number string-length]))))
         (displayln
          (syntax->datum
           (replace-in-instance #'(List Symbol String)
                                     #'([String Number string-length])))))
       #;(define-syntax (string→number stx)
           (replace-in-data-structure
            #'(List (Pairof Symbol String))
            #'[String Number string-length]))]

@CHUNK[<replace-in-data-structure>
       (define-for-syntax (replace-in-data-structure t r)
         (define (recursive-replace new-t) (replace-in-data-structure new-t r))
         (define/with-syntax ([from to fun] ...) r)
         (syntax-parse t
           [x:id
            #:attr assoc-from-to (cdr-stx-assoc #'x #'((from . to) ...))
            #:when (attribute assoc-from-to)
            #'assoc-from-to]
           [((~literal List) a ...)
            #`(List #,@(stx-map recursive-replace #'(a ...)))]
           [((~literal Pairof) a b)
            #`(Pairof #,(recursive-replace #'a) #,(recursive-replace #'b))]
           [((~literal Listof) a)
            #`(Listof #,(recursive-replace #'a))]
           [((~literal Vector) a ...)
            #`(Vector #,@(stx-map recursive-replace #'(a ...)))]
           [((~literal Vectorof) a)
            #`(Vectorof #,(recursive-replace #'a))]
           [((~literal U) a ...)
            #`(U #,@(stx-map recursive-replace #'(a ...)))]
           [x:id #'x]))]

@CHUNK[<replace-in-instance>
       (define-for-syntax (replace-in-instance t r)
         (define/with-syntax ([from to fun] ...) r)
         (define (recursive-replace type)
           (syntax-parse type
           [x:id
            #:attr assoc-from-to (cdr-stx-assoc #'x #'((from . (to . fun)) ...))
            #:when (attribute assoc-from-to)
            #:with (to-type . to-fun) #'assoc-from-to
            (define/with-syntax (tmp) (generate-temporaries #'(x)))
            ;; TODO: Add predicate for to-type in the pattern.
            (cons #`(and tmp) #`(to-fun tmp))]
           [((~literal List) a ...)
            (define/with-syntax (tmp ...) (generate-temporaries #'(a ...)))
            (define rec (stx-map recursive-replace #'(a ...)))
            (cons #`(list #,@(map car rec))
                  #`(list #,@(map cdr rec)))]
           [((~literal Pairof) a b)
            (define/with-syntax (tmpa tmpb) (generate-temporaries #'(a b)))
            (define reca (recursive-replace #'a))
            (define recb (recursive-replace #'b))
            (cons #`(cons #,(car reca) #,(car recb))
                  #`(cons #,(cdr reca) #,(cdr recb)))]
           #| TODO:
           [((~literal Listof) a)
            #`(Listof #,(recursive-replace #'x))]
           [((~literal Vector) a ...)
            #`(Vector #,@(stx-map recursive-replace #'(a ...)))]
           [((~literal Vectorof) a)
            #`(Vectorof #,(recursive-replace #'a))]
           |#
           #|
           [((~literal U) a ...)
            ;; Use (app (λ _ 'a) U-case) to set U-case, so that we can do a
            ;; very simple cond in the replacement.
            ;; TODO: write a `bind` match-expander, much like syntax-parse's
            ;; ~bind.
            (define/with-syntax (tmp ...) (generate-temporaries #'(a ...)))
            (cons #`(or (app (λ _')]
           ;; DOES NOT ACTUALLY WORK, because match wants all `or` branches to
           ;; have the same variables.
           |#
           [x:id
            (define/with-syntax (tmp) (generate-temporaries #'(x)))
            (cons #'tmp #'tmp)]))
         (define whole-rec (recursive-replace t))
         #`(λ (v) (match-abort v [#,(car whole-rec) #,(cdr whole-rec)])))]

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
                    "../type-expander/type-expander.lp2.rkt"
                    "cond-abort.rkt")
           (begin-for-syntax (provide replace-in-data-structure
                                      replace-in-instance))
           
           <replace-in-data-structure>
           <replace-in-instance>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    typed/rackunit
                    "structure.lp2.rkt"
                    "variant.lp2.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "cond-abort.rkt")
           
           <test-example>
           
           (require (submod ".." doc))))]