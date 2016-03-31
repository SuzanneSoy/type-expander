#lang scribble/manual

@(require (for-label typed/racket/base
                     syntax/parse
                     ;"template.rkt"
                     ))

@(define ellipses (racket ...))

@title{Versatile parser and template library}

Keywords: grammar, parser, template.

@defform[(parse expr [pattern body …] …)]{
 Analogous to @racket[syntax-parse], except it isn't
 specialized for syntax, but rather works for arbitrary
 s-expressions, including syntax ones (denoted by 
 @racket[#'(…)] in the pattern).}

@defform[#:literals (: :: ... else struct)
         (tmpl template)
         #:grammar
         [(template variable
                    [variable : type] ;; (ann variable type)
                    ;; cons-template
                    (template . template)
                    (template :: template)
                    
                    ;; list
                    (template**)
                    ;; list*
                    template**-dotted
                    
                    ;; vector
                    #(template**)
                    (vector . template**-dotted)
                    
                    ;; hash-template: template** must expand to a list of pairs.
                    (hash . template**-dotted)   ;; TODO: how to distinguish
                    (hasheq . template**-dotted) ;; mutable and immutable?
                    (hasheqv . template**-dotted)
                    #hash([template . template])
                    #hasheq([template . template])
                    #hasheqv([template . template])
                    
                    ;; struct-template
                    (struct-id template …)
                    (struct struct-id template …)
                    #s(prefab-id template …)
                    #s(template template …) ;; Only allowed in untyped racket

                    ;; box
                    #&template
                    
                    ;; call-template
                    (~identifier args …) ;; calls (identifier args …)
                    (~ expr args …)      ;; calls (expr args …)
                    
                    ;; unquote-template
                    ,expr
                    ,@(list expr)
                    ,@(list* expr) ;; must appear in last position.
                    
                    
                    ;; template-expander
                    template-expander-id
                    (template-expander-id args …)
                    
                    ;; maybe-template (should all be template expanders
                    ;; which means the system is extensible enough to express
                    ;; these special cases).
                    (?? alt-template …)
                    (?@ . template**-dotted)
                    (??@ . template**-dotted)
                    (?if condition template template)
                    (|@if| condition template template)
                    (if@ condition template template)
                    (|@cond| [condition template] …)
                    (|@cond| [condition template] … [else template])
                    (cond@ condition template template)
                    
                    ;; like #,@(with-syntax ([meta-var #'template])
                    ;;           #'(template**))
                    (~let ([meta-var+args template])
                          . template**-dotted)
                    
                    (~sort key template ooo)
                    (~loc stxloc . template)
                    
                    ;; escaped
                    (ddd escaped)
                    
                    ;; 
                    
                    ;; literal
                    #t
                    #f
                    string
                    bytes
                    number
                    char
                    keyword
                    regexp
                    pregexp)

          (meta-var+args meta-var
                         (meta-var meta-arg …))
          
          (tail-template template)
          
          ;; specialize mid-sequence in repetition (diagonal-matrix-style)
          
          (variable identifier)

          (template**-dotted (template* … . template)
                             template**)
          (template** (code:line template* …)
                      (code:line template* … :: template)
                      (code:line template* … (~rest . template)))
          (template* template
                     (code:line template ooo)
                     special-cased-template)
          (special-cased-template (code:line template vardd)
                                  (code:line template ddvar))
          ;; Where var is an iterated variable.
          (vardd var..   ;; exclude the current iteration
                 var...) ;; include the current iteration
          (ddvar ..var   ;; exclude the current iteration
                 ...var) ;; include the current iteration
          
          (ooo #,ellipses ;; TODO: make it a hyperlink
               ___
               ..k ;; k positive integer
               __k ;; k positive integer
               (code:line .. expr)  ;; expr must return a positive integer
               (code:line __ expr)) ;; expr must return a positive integer
          (ddd #,ellipses)
          ]]{
 TODO: implement the versatile template library. 
 @racket[...]
 
 TODO: support for typed/racket.
 
 The patterns for @racket[parse] should all have a way to
 create a symmetric counterpart for @racket[tmpl], which
 produces the original value. This symmetry is important
 because allows lens-like macros, which operate on only part
 of the data structure, leaving everything else intact.
 
 @racket[??] works like @racket[??] from 
 @racket[syntax/parse/experimental/template], except it
 allows any number of alternatives (including 0, to avoid
 special-casing in macros). It is more or less equivalent to
 @racket[(?? a (?? b (?? c …)))], following syntax/parse's
 semantics.
 
 @racket[?@] has the same meaning as in syntax/parse.
 
 @racket[(??@ t* …)] is a shortcut for 
 @racket[(?? (?@ t* …))]
 
 For better compatibility with at-exp, @racket[|@if|] can be
 written @racket[if@], and the same goes for 
 @racket[|@cond|] etc.
 
 TODO: what's the difference between @racket[~], 
 @racket[template-expander] and @racket[unquote]? 
 @racket[template-expander] runs at compile-time and should
 treat its arguments as syntax.
 
 Concerning unquoting, unlike @racket[racket]'s default
 behaviour in @RACKET[#'([x #,(y …)] …)], unquoting should
 not break the nesting of ellipses. How should we express
 voluntary variation of the level of nesting? @racket[~let]
 already allows expanding part of the template at some level
 and inserting it verbatim somewhere below, but it's not a
 silver bullet. One case which comes to mind is when some of
 the nested data should be mixed with less-nested data, for
 example going from 
 @racket[([10 1 2 3] [100 4 5] [1000 6])] to 
 @racket[([10 20 30] [400 500] [6000])] should be relatively
 easy to express. Maybe @racket[~let] with parameters can be
 a suitable generalized solution: 
 @RACKET[({~let ([(addx v) #,(+ x v)]) [(addx y) …]} …)]
 
 The special-cased template syntax should allow special
 treatment of the @racket[i]-th iteration in a doubly-nested
 loop: matching @racket[x] on @racket[(1 2 3 4 5)], and
 using the template @racket[(0 x.. ,(* x x) ..x 1) …] will
 produce @racket[(1 1 1 1 1)
                 (0 4 1 1 1)
                 (0 0 9 1 1)
                 (0 0 0 16 1)
                 (0 0 0 0 24)]. The pattern before 
 @racket[x..] and the pattern after @racket[..x] can expand
 to multiple items which will be spliced in by wrapping it
 with @racket[?@].}

@section{Ideas for implementation}

@subsection{Extensibility (expanders)}

Allow normal, inline-prefix, inline-postfix and inline-infix
expanders, which can bind using regular expressions. This
allows implementing exotic syntax like @racket[var..]
(postfix, operates on the pattern preceeding it), 
@racket[..var] (postfix, operates on the pattern after it),
@racket[(… escaped-pattern)] (normal, operates on the
containing s-exp)

@subsection{Customization}

For things that are likely to be customized by the user in
the whole file scope, define a grammar/custom module, used
as follows:

@racketblock[(require grammar/custom)
             (grammar/custom option …)]

The @racket[grammar/custom] macro expands to 
@racket[(require grammar/core)] followed by a bunch of 
@racket[define-syntax] which wrap the core macros, providing
them the custom options:

@racketblock[(require grammar/core)
             (define-syntax-rule (parse . rest)
               (parse/core #:global-options (option …) . rest))
             (define-syntax-rule (tmpl . rest)
               (parse/core #:global-options (option …) . rest))]

This can also be used to rename the @racket[parse] and 
@racket[tmpl] macros, if desired (for example, 
@racket[tmpl] could be renamed to @racket[quasisyntax], or
something similar).

Or maybe we should just use units? Can they be customized in
a similar way?

The idea is to avoid having to wrap the whole file in a 
@racket[(parameterize …)], and be able to easily 
@racket[provide] a customized variation of this library:

@racketblock[(provide (customized-out grammar/custom))]

@subsection{Things to look at}

@itemlist[
 @item{@racket[math/arry], for @racket[::] and array
  broadcasting.}
 @item{Quasipatterns in @racket[match].}
 @item{The @racket[lens] library}]

