#lang scribble/lp2
@(require "../../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Life, the Universe and Everything.}

@(table-of-contents)

@section{Introduction}

@chunk[<foo>
       (define foo 42)]

Here is a macro:

@CHUNK[<scribble-macro-expansion>
       (define-for-syntax mymacro-tmp
         (syntax-rules () [(_ a b) (let ((b 1)) a)]))
       (define-syntax (mymacro-stx stx) #`'#,(mymacro-tmp stx))
       (provide mymacro-stx)
       (define-syntax mymacro mymacro-tmp)]

We can use it like this:

@chunk[<scribble-macro-expansion-example>
       (mymacro (+ x 3) x)]

Which expands to (requires a bit of set-up boilerplate to have the output in
scribble, see
@url{http://lists.racket-lang.org/users/archive/2014-December/065175.html}):
@(begin
   (require syntax/location scribble/eval)
   (define here (quote-source-file))
   (define evaluator (make-base-eval #:lang 'typed/racket))
   (evaluator `(begin
                 (require (for-syntax racket/base))
                 (dynamic-require '(file ,here) #f)
                 (current-namespace
                  (module->namespace '(file ,here))))))

@interaction[#:eval evaluator
             (mymacro-stx (+ x 3) x)]

@chunk[<test-foo>
       (check-equal? foo 42)]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              racket/syntax
                              "../../lib/low-untyped.rkt")
                  "../../lib/low-untyped.rkt")
         (provide foo)
         
         <foo>
         <scribble-macro-expansion>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  typed/rackunit)
         
         <test-foo>
         
         (require (submod ".." doc)))]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]