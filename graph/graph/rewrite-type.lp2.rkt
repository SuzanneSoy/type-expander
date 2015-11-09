#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Rewriting data structures and their types}

This module allows purely functional substitution inside a data structure of
arbitrarily deep elements of a given type, while also computing the type of the
result.

For example, one could replace all strings in a data structure by their length:

@CHUNK[<test-example>
       (define-syntax (make-replace stx)
         (syntax-case stx ()
           [(_ name type . replace)
            (displayln (syntax->datum #`(begin
                                          (: name (→ type #,(replace-in-data-structure #'type #'replace)))
                                          (define (name v)
                                            #,(replace-in-instance #'v #'type #'replace)))))
            #'(list)]))
       
       (make-replace test1
                     (List (Pairof (List Symbol (Listof String)) String))
                     [String Number string-length])
       
       ;(test1 '((#(sym ("ab" "abc" "abcd")) . "a")))
       
       (begin-for-syntax 
         #;(displayln
            (syntax->datum
             (replace-in-instance #'v
                                  #'(List (Pairof (Vector Symbol
                                                          (Vectorof String))
                                                  String))
                                  #'([String Number string-length]))))
         (displayln
          (syntax->datum
           (replace-in-instance #'v
                                #'(List Symbol String)
                                #'([String Number string-length])))))
       (define-syntax (string→number stx)
         #`(define-type new-t
             #,(replace-in-data-structure
                #'(List (Pairof (Vector Symbol (Vectorof String)) String))
                #'([String Number string-length]))))
       
       (string→number)]

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
       (define-for-syntax (replace-in-instance val t r)
         (define/with-syntax ([from to fun] ...) r)
         (define (recursive-replace stx-val type)
           (define/with-syntax val stx-val)
           (syntax-parse type
             [x:id
              #:attr assoc-from-to (cdr-stx-assoc #'x
                                                  #'((from . (to . fun)) ...))
              #:when (attribute assoc-from-to)
              #:with (to-type . to-fun) #'assoc-from-to
              (define/with-syntax (tmp) (generate-temporaries #'(x)))
              ;; TODO: Add predicate for to-type in the pattern.
              #`(match-abort val [(and tmp) (protected (to-fun tmp))])]
             [((~literal List) a ...)
              (define/with-syntax (tmp1 ...) (generate-temporaries #'(a ...)))
              (define/with-syntax (tmp2 ...) (generate-temporaries #'(a ...)))
              (define/with-syntax (rec ...)
                (stx-map recursive-replace #'(tmp1 ...) #'(a ...)))
              #`(match-abort val
                  [(list tmp1 ...)
                   (let-abort ([tmp2 rec] ...)
                              (protected (list (unprotect tmp2) ...)))])]
             [((~literal Pairof) a b)
              (define/with-syntax (tmpa1 tmpb1) (generate-temporaries #'(a b)))
              (define/with-syntax (tmpa2 tmpb2) (generate-temporaries #'(a b)))
              (define/with-syntax reca (recursive-replace #'tmpa1 #'a))
              (define/with-syntax recb (recursive-replace #'tmpb1 #'b))
              #'(match-abort val
                  [(cons tmpa1 tmpb1)
                   (let-abort ([tmpa2 reca] [tmpb2 recb])
                              (protected (cons (unprotect tmpa2)
                                               (unprotect tmpb2))))])]
             #| TODO: |#
             [((~literal Listof) a)
              (define/with-syntax (tmp1) (generate-temporaries #'(a)))
              (define/with-syntax (tmp1x) (generate-temporaries #'(a)))
              (define/with-syntax rec (recursive-replace #'tmp1x #'a))
              #'(match-abort val
                  [(list tmp1 (... ...))
                   (map-abort tmp1 tmp1x rec)])]
             [((~literal Vector) a ...)
              (define/with-syntax (tmp1 ...) (generate-temporaries #'(a ...)))
              (define/with-syntax (tmp2 ...) (generate-temporaries #'(a ...)))
              (define/with-syntax (rec ...)
                (stx-map recursive-replace #'(tmp1 ...) #'(a ...)))
              #`(match-abort val
                  [(vector tmp1 ...)
                   (let-abort ([tmp2 rec] ...)
                              (protected (list (unprotect tmp2) ...)))])]
             #| TODO:
             [((~literal Vectorof) a)
              #`(Vectorof #,(recursive-replace #'a))]
             ;|#
             [((~literal U) a ...)
              (define/with-syntax (tmp1 ...) (generate-temporaries #'(a ...)))
              (define/with-syntax (rec ...)
                (stx-map recursive-replace #'(tmp1 ...) #'(a ...)))
              #`(match-abort val
                  [tmp1 rec]
                  ...)]
             [x:id
              #'(protected val)]))
         ;; TODO: if we recieve a 'continue or 'break, give a type error.
         #`(unprotect #,(recursive-replace val t)))]

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