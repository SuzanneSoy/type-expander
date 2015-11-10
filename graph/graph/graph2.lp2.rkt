#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Graph library}

@(table-of-contents)

@section{Introduction}



@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax syntax/parse
                                racket/syntax
                                "../lib/low-untyped.rkt")
                    "../lib/low.rkt")

           )
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    typed/rackunit)

           
           
           (require (submod ".." doc))))]