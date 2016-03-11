#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Implementation of structures}

@(table-of-contents)

@section{@racket[define-structure]}

The @tc[remember-all] for-syntax function below memorizes its arguments across
compilations, and adds them to the file “@code{remember.rkt}”:

@CHUNK[<remember-all>
       (require "remember.rkt")
       
       (define (check-remember-all category value)
         (let ([datum-value (syntax->datum (datum->syntax #f value))])
           (if (not (member (cons category datum-value) all-remembered-list))
               (let ((file-name (build-path (this-expression-source-directory)
                                            "remember.rkt")))
                 ;; Add the missing field names to all-fields.rkt
                 (with-output-file [port file-name] #:exists 'append
                                   (writeln (cons category datum-value) port))
                 #f)
               #t)))]

@CHUNK[<remember-all-errors>
       (define (remember-all-errors id fallback stx)
         ;<remember-all-hard-error>
         #`(#,id #,(for/list ([cause `(,stx ,fallback)])
                     (syntax/loc cause delayed-error-please-recompile))))
       
       (define remember-err-id-list '())
       
       (define (remember-all-errors2 placeholder cause-stx)
         (let ((file-name (build-path (this-expression-source-directory)
                                      "remember.rkt")))
           (set! remember-err-id-list
                 (cons cause-stx remember-err-id-list))
           (syntax-local-lift-module-end-declaration
            #`(remember-all-hard-error #,file-name))
           placeholder))]

@CHUNK[<remember-all-hard-error>
       (define-syntax (remember-all-hard-error stx)
         (syntax-case stx ()
           ([whole-stx file-name]
            (raise-syntax-error
             'remember-all
             (format (~a "I added the identifiers ~a to my remembered list in"
                         " ~a . Please recompile now.")
                     (string-join (stx-map identifier->string
                                           remember-err-id-list)
                                  ", ")
                     (syntax->datum #'file-name))
             #f
             #f
             remember-err-id-list))))]

We can, during subsequent compilations, retrieve the list of already-memorized
fields for a given tag.

@CHUNK[<get-remembered>
       (define (get-remembered category)
         (cdr (or (assoc category all-remembered-alist) '(_))))]

If we start with an empty “@code{remember.rkt}” file, it will throw an error at
each call with a not-yet-remembered value. In order to avoid that, we use the
macro @tc[(delayed-error-please-recompile)], which expands to an undefined
identifier @code{please-recompile}. That error is caught later, and gives a
chance to more calls to @tc[remember-all] to be executed during macro-expansion.
We define @tc[delayed-error-please-recompile] in a submodule, to minimize the
chances that we could write a definition for that identifier.

The @tc[delayed-error-please-recompile] macro has to be
declared in a @tc[typed/racket] module so that it can be
included in @tc[typed/racket] modules too.

@itemlist[
 @item{TODO: do we really need this? It is going to trigger
  an error after all!}
 @item{TODO: could we instead use 
  @racket[(syntax-local-lift-module-end)] to push a macro
  throwing a @racket[raise-syntax-error] right to the end of
  the module? We could this way memorize all failed
  querries, and highlight them using 
  @racket[raise-syntax-error].}]

@CHUNK[<delayed-error-please-recompile>
       (begin
         (module m-please-recompile typed/racket
           (define-syntax (delayed-error-please-recompile stx)
             #'please-recompile)
           (provide delayed-error-please-recompile))
         
         (require (for-template 'm-please-recompile)))]

Due to a bug in scribble, the above has to be wrapped in a 
@tc[begin] form, otherwises the @tc[require] statement can't
access the previously declared module.

The functions above are easier to define in a 
@tc[begin-for-syntax] environment, as 
@tc[remember-all-errors2] refers to the 
@tc[remember-all-hard-error] macro, in the template
@tc[(syntax-local-lift-module-end-declaration
     #`(remember-all-hard-error …))], and that macro
accesses mutable @tc[remember-err-id-list] and 
@tc[remember-err-stx-list]. It is therefore much simpler to
define everything in the same module, in a 
@tc[begin-for-syntax] block except the macro which will be
declared using @tc[define-syntax].

@chunk[<for-syntax-declarations>
       (require mzlib/etc
                (submod "../lib/low.rkt" untyped)
                (for-syntax mzlib/etc
                            (submod "../lib/low.rkt" untyped)
                            racket/string
                            racket/format))
       (begin-for-syntax
         (provide check-remember-all
                  remember-all-errors
                  get-remembered
                  remember-all-errors2)
         
         <delayed-error-please-recompile>
         <remember-all>
         <remember-all-errors>
         <get-remembered>)
       ;; remember-all-hard-error is a define-syntax.
       <remember-all-hard-error>]

We would however like to be able to 
@racket[(require "remember-lib.lp2.rkt")] and have the
bindings it provides available at the same meta-level. We
therefore define the bindings above in a separate submodule,
@racket[require] it @tc[for-template] which has the efect of
shifting the meta-level of all the bindings one level down,
and re-provide the bindings which are now at meta-level @tc[0].

@chunk[<*>
       (begin
         (module for-syntax-declarations racket
           <for-syntax-declarations>)
         (module main racket
           (require (for-template (submod ".." for-syntax-declarations)))
           (provide check-remember-all
                    remember-all-errors
                    get-remembered
                    remember-all-errors2))
         (require 'main)
         (provide (all-from-out 'main)))]