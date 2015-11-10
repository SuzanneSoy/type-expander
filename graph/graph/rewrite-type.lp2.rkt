#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Rewriting data structures and their types}

@section[#:tag "sec:intro-example"]{Introductory example}
This module allows purely functional substitution inside a data structure of
arbitrarily deep elements of a given type, while also computing the type of the
result.

For example, one could replace all strings in a data structure by their length:

@CHUNK[<test-example>
       (make-replace test-example
                     (Vectorof (U (List 'tag1 String) (List 'tag2 Number)))
                     [String Number string-length])]

The result's type would be derived from the original one, but all occurrences of
@tc[String] have been replaced by @tc[Number]. The result itself would have the
value returned by string-length instead of each string, everything else being
identical.

@CHUNK[<test-example>
       (check-equal?
        (ann (test-example '#((tag1 "a") (tag2 7) (tag1 "bcd")))
             (Vectorof (U (List 'tag1 Number) (List 'tag2 Number))))
        '#((tag1 1) (tag2 7) (tag1 3)))]

In this example, we used @tc[make-replace], a test macro defined below which
relies on the lower-level utilities provided by this module, namely
@tc[replace-in-type] and @tc[replace-in-instance].

@CHUNK[<test-make-replace>
       (define-syntax (make-replace stx)
         (syntax-case stx ()
           [(_ name type [from to fun] ...)
            #`(begin
                (: name (→ type #,(replace-in-type #'type #'([from to] ...))))
                (define (name v)
                  #,(replace-in-instance #'v
                                         #'type
                                         #'<make-replace-wrapped-fun>)))]))]

@CHUNK[<make-replace-wrapped-fun>
       ([from to (λ ([x : from] [acc : Any]) (fun x))]
        ...)]

@subsection{A bigger example}

We would expect this to work on bigger types without any extra efforts. In the
following example, we replace all strings with their length, on a bigger
example:

@CHUNK[<test-big>
       (make-replace test-big
                     (List (Pairof (U (List 'tag1 (List (Vector Symbol)
                                                        Number
                                                        (Listof String)))
                                      (List 'tag2 (List (Vector Symbol)
                                                        Number
                                                        (Listof String))))
                                   String))
                     [String Number string-length])]

The replacement function @tc[test-big] defined above will, as expected, have a
return type containing no more strings, and the correct return value.

@CHUNK[<test-big>
       (check-equal?
        (ann (test-big '(((tag2 (#(sym) 7 ("ab" "abc" "abcd"))) . "a")))
             (List (Pairof (U (List 'tag1 (List (Vector Symbol)
                                                Number
                                                (Listof Number)))
                              (List 'tag2 (List (Vector Symbol)
                                                Number
                                                (Listof Number))))
                           Number)))
        '(((tag2 (#(sym) 7 (2 3 4))) . 1)))]

@section{Replacing parts of a type}

The @tc[replace-in-type] @tc[for-syntax] function is pretty straightforward: it
checks whether the given type matches one of the substitution rules given in
@tc[r]. If no substitution rule was found, it matches the type against a small
set of known type constructors like @tc[List] or @tc[Pairof], and recursively
calls itself on the components of the type.

@CHUNK[<replace-in-type>
       (define-for-syntax (replace-in-type t r)
         (define (recursive-replace new-t) (replace-in-type new-t r))
         (define/with-syntax ([from to] ...) r)
         (syntax-parse t
           <replace-in-type-substitute>
           <replace-in-type-other-cases>))]

The clause that matches the type against the substitution rules uses the
@tc[stx-assoc] function defined in our library, which uses
@tc[free-identifier=?] to find the first pair which @tc[car] or @tc[stx-car]
matches the given type @tc[#'x].

@CHUNK[<replace-in-type-substitute>
       [x:id
        #:attr assoc-from-to (cdr-stx-assoc #'x #'((from . to) ...))
        #:when (attribute assoc-from-to)
        #'assoc-from-to]]

The other cases use @tc[~literal] and a syntax pattern to find uses of
@tc[List], @tc[Pairof], @tc[Vectorof] etc.

@CHUNK[<replace-in-type-other-cases>
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
       <replace-in-type-case-quote>
       [x:id
        #'x]]

TODO: If the type is a quoted primitive, we should replace it too, for example
@tc['("abc" symbol)] should be transformed into @tc['(3 symbol)] if we apply the
@tc[[String Number string-length]] substitution from the example in section
@secref{sec:intro-example}.

@CHUNK[<replace-in-type-case-quote>
       [((~literal quote) a)
        #`(quote a)]]

@section{Replacing parts of an instance}

The @tc[replace-in-instance] for-syntax function is defined in a similar way,
with an internal definition for @tc[recursive-replace]. The case of unions is
offloaded to a separate subroutine.

@CHUNK[<replace-in-instance>
       (define-for-syntax (replace-in-instance val t r)
         (define/with-syntax ([from to fun] ...) r)
         <recursive-replace-in-instance>
         <replace-in-union>
         (recursive-replace val t))]

The @tc[recursive-replace] internal function defined below takes a type
@tc[type] and produces an expression that transforms instances of that type
using the substitution rules given in the parameter @tc[r] of the enclosing
function.

The expression assumes that the instance to be transformed is located in the
variable or expression @tc[stx-val], and caching is used where needed to avoid
evaluating @tc[stx-val] twice. Here is the case that handles @tc[Pairof], which
caches @tc[val] and calls @tc[recursive-replace] with the @tc[car] and @tc[cdr]
as expressions:

@CHUNK[<replace-in-instance-case-pairof>
       [((~literal Pairof) a b)
        #`(let ([v-cache val])
            (cons #,(recursive-replace #'(car v-cache) #'a)
                  #,(recursive-replace #'(cdr v-cache) #'b)))]]

The other cases are similarly defined:

@CHUNK[<recursive-replace-in-instance>
       (define (recursive-replace stx-val type)
         (define/with-syntax val stx-val)
         (define/with-syntax (v-cache) (generate-temporaries #'(val-cache)))
         (syntax-parse type
           [x:id
            #:attr assoc-from-to (cdr-stx-assoc #'x #'((from . (to . fun)) ...))
            #:when (attribute assoc-from-to)
            #:with (to-type . to-fun) #'assoc-from-to
            (define/with-syntax (tmp) (generate-temporaries #'(x)))
            ;; TODO: Add predicate for to-type in the pattern.
            #`(to-fun val '())]
           [((~literal List) a ...)
            (define/with-syntax (tmp ...) (generate-temporaries #'(a ...)))
            #`(let-values ([(tmp ...) (apply values val)])
                (list #,@(stx-map recursive-replace #'(tmp ...) #'(a ...))))]
           <replace-in-instance-case-pairof>
           [((~literal Listof) a)
            (define/with-syntax (tmp) (generate-temporaries #'(a)))
            #`(map (λ ([tmp : a]) #,(recursive-replace #'tmp #'a))
                   val)]
           [((~literal Vector) a ...)
            (define/with-syntax (tmp ...) (generate-temporaries #'(a ...)))
            (define/with-syntax (idx ...) (generate-indices #'(a ...)))
            #`(let ([v-cache val])
                (let ([tmp (vector-ref v-cache idx)]
                      ...)
                  (vector #,@(stx-map recursive-replace
                                      #'(tmp ...)
                                      #'(a ...)))))]
           [((~literal Vectorof) a)
            (define/with-syntax (tmp) (generate-temporaries #'(a)))
            #`(vector-map (λ ([tmp : a]) #,(recursive-replace #'tmp #'a))
                          val)]
           [((~literal U) a ...)
            #`(let ([v-cache val])
                (cond
                  #,@(stx-map (λ (ta) (replace-in-union #'v-cache ta r))
                              #'(a ...))))]
           [((~literal quote) a)
            #'val]
           [x:id
            #'val]))]

For unions, we currently support only tagged unions, that is unions where each
possible type is a @tc[List] with a distinct @tc[tag] in its first element.
TODO: we currently don't check that each @tc[tag] is distinct.

@CHUNK[<replace-in-union>
       (define (replace-in-union stx-v-cache t r)
         (define/with-syntax v-cache stx-v-cache)
         (syntax-parse t
           [(List ((~literal quote) tag:id) b ...)
            <replace-in-tagged-union-instance>]
           [_ (error "Type-replace on untagged Unions isn't supported yet!")]))]

For cases of the union which are a tagged list, we use a simple guard, and call
@tc[recursive-replace] on the whole @tc[(List 'tag b ...)] type.

@CHUNK[<replace-in-tagged-union-instance>
       #`[(and (list? v-cache)
               (not (null? v-cache))
               (eq? 'tag (car v-cache)))
          #,(recursive-replace #'v-cache t)]]

Handling freer forms of unions causes some problems:

@itemlist[
 @item{There are some types without make-predicate, so we can't use
  @racket[(make-predicate T)] to know what case of the union we are in}
 @item{There are some types for which there is no predicate, like function types
  with the same arity}
 @item{There are some types for which the type system doesn't acknowledge the
  predicates, e.g. @racket[(U (Vector Number) (Vector String String))]: we can't
  even bound the type of @racket[(vector-ref x 0)] in that case, it defaults to
  @racket[Any].}]

These issues and possible solutions are addressed in more
detail in the 
@hyperlink[(string-append "https://phc.fogbugz.com/f/cases/54/"
                          "Rethink-how-to-do-the-multi-step-types-more-inside")]
{FogBugz case 54}.

@section{Extracting parts of an instance}

Replacing parts of an instance may require first extracting them. We define here
a general fold over some data structures, that allows the replacement function
to know a value returned by previous replacements. It can be easily adapted to
have each substitution have a different accumulator by using @tc[list] or
@tc[struct] of these accumulators as the main one.

The order in which the elements of the structure are passed to the substitution
functions is undefined.

@subsection{Tests}

@CHUNK[<test-fold-instance>
       (make-fold test-fold
                  (List String Number (List String String Symbol String))
                  Number
                  [String Number (λ ([x : String] [acc : Number])
                                   (values (+ (string-length x) acc)
                                           (+ acc 1)))])
       
       (test-fold '("a" 7 ("b" "c" x "d")) 0)]

@CHUNK[<test-make-fold>
       (define-syntax (make-fold stx)
         (syntax-case stx ()
           [(_ name type acc-type [from to fun] ...)
            #`(begin
                (: name (→ type
                           acc-type
                           (values #,(replace-in-type #'type #'([from to] ...))
                                   acc-type)))
                (define name
                  #,(fold-instance #'v
                                   #'type
                                   #'acc-type
                                   #'([from to fun] ...))))]))]

@subsection{The code}

@CHUNK[<fold-instance>
       (define-for-syntax (fold-instance val t stx-acc-type r)
         (define/with-syntax acc-type stx-acc-type)
         (define/with-syntax ([from to fun] ...) r)
         <recursive-replace-fold-instance>
         ;<replace-fold-union>
         (recursive-replace t))]

@CHUNK[<recursive-replace-fold-instance>
       (define (recursive-replace type)
         (define/with-syntax (v-cache) (generate-temporaries #'(val-cache)))
         (syntax-parse type
           [x:id
            #:attr assoc-from-to (cdr-stx-assoc #'x #'((from . (to . fun)) ...))
            #:when (attribute assoc-from-to)
            #:with (to-type . to-fun) #'assoc-from-to
            (define/with-syntax (tmp) (generate-temporaries #'(x)))
            ;; TODO: Add predicate for x-to in the pattern.
            #`to-fun]
           [((~literal List) a ...)
            (define/with-syntax (tmp1 ...) (generate-temporaries #'(a ...)))
            (define/with-syntax (tmp2 ...) (generate-temporaries #'(a ...)))
            (define/with-syntax (new-acc ...) (generate-temporaries #'(a ...)))
            (define/with-syntax (new-acc1 ... new-acc-last) #'(acc new-acc ...))
            (define/with-syntax (rec ...)
              (stx-map recursive-replace #'(a ...)))
            #`(λ ([val : (List a ...)] [acc : acc-type])
                (let*-values ([(tmp1 ...) (apply values val)]
                              [(tmp2 new-acc) (rec tmp1 new-acc1)]
                              ...)
                  (values (list tmp2 ...) new-acc-last)))]
           [x:id
            #'values]))]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax syntax/parse
                                racket/syntax
                                syntax/stx
                                "../lib/low-untyped.rkt")
                    "structure.lp2.rkt"
                    "variant.lp2.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "cond-abort.rkt")
           (begin-for-syntax (provide replace-in-type
                                      replace-in-instance
                                      fold-instance))
           
           <replace-in-type>
           <replace-in-instance>
           <fold-instance>)
         
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
           
           <test-make-replace>
           <test-example>
           <test-big>
           
           <test-make-fold>
           <test-fold-instance>
           
           (require (submod ".." doc))))]