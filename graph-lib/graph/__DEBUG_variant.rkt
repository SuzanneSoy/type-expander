#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Variants}

@(table-of-contents)

@section{Introduction}

@chunk[<mainbody>
       (constructor de1 2 "y")
       (constructor de2 2 "y")
       (constructor de3 2 "y")]

@chunk[<testbody>
       ;; check-equal?: and ann (for the value part only, not for the type part)
       ;; these break the error reporting mechanism, and we only have
       ;; "please-recompile: unbound identifier in module".
       (check-equal?: (ann (constructor dh1 2 "y")
                           (constructor dh1 Number String))
                      (constructor dh1 2 "y"))
       
       (define-tagged txyz #:private #:? txyz?
         [a Number]
         [b String])
       
       (ann (constructor dk1 2 "y")
            (constructor dk1 Number String))]

@chunk[<*>
       (begin 
         (module main typed/racket
           (require (for-syntax racket/list)
                    "adt.lp2.rkt")
           <mainbody>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    typed/rackunit
                    "adt.lp2.rkt")
           
           <testbody>))]