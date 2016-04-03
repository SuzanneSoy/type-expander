#lang scribble/manual

@(require scribble-enhanced/manual-form)

@(require (for-label typed/racket/base
                     "rewrite-type.lp2.rkt"))

@title{Rewrite-type utilities for writing type-level functions}

The utilities described here allow replacing parts of a
type with other types, and generating conversion functions
transforming instances of the old type into instances of the
new type.

@defform[#:kind "procedure"
         (replace-in-type old-type
                          #'([from to] …))
         #:result (syntax-for type)
         #:contracts ([old-type (syntax-for type)]
                      [from (and/c identifier? (syntax-for type))]
                      [to (syntax-for type)])]{
 This type-level function produces the syntax for the 
 @racket[new-type]. The @racket[new-type] has the same shape
 as the @racket[old-type], except all occurrences of the 
 @racket[from] type (which must be just a single identifier)
 replaced with the @racket[to] type in the type.
 
 The @racket[replace-in-type] type-level function first
 expands any @racketmodname[type-expander]s, and performs
 the replacements on the expanded type.}

@deftogether[
 (@defform[#:kind "procedure"
           (replace-in-instance old-type
                                #'([from to pred? fun] …))
           #:result (syntax-for (→ old-type new-type))]
   @defform[#:kind "template metafunction"
            (tmpl-replace-in-instance old-type
              [from to pred? fun] …)
            #:result (syntax-for (→ old-type new-type))
            #:contracts ([old-type (syntax-for type)]
                         [from (and/c identifier? (syntax-for type))]
                         [to (syntax-for type)]
                         [pred? (syntax-for predicate?)]
                         [fun (syntax-for (→ from to))])])]{
 Produces the syntax for the syntax for a function from the
 @racket[old-type] to the @racket[new-type], transforming
 all parts of the data structure which satisfy 
 @racket[pred?] using @racket[fun]. The @racket[new-type]
 will be the same as the one that would be returned by 
 @racket[(replace-in-type old-type #'([from to] …))]
 
 @racket[pred?] should return true if and only if the data
 passed as an argument is an instance of the @racket[from]
 type. @racket[fun] should accept instances of the 
 @racket[from] type, and return instances of the 
 @racket[to] type.
}

@deftogether[
 (@defform[#:kind "procedure"
           (fold-instance old-type
                          accumulator-type
                          ([from to pred? fun] …))
           #:result (syntax-for
                     (→ old-type (Values new-type accumulator-type)))]
   @defform[#:kind "template metafunction"
            (tmpl-fold-instance old-type accumulator-type
              [from to pred? fun] …)
            #:result (syntax-for
                      (→ old-type (Values new-type accumulator-type)))
            #:contracts ([old-type (syntax-for type)]
                         [accumulator-type (syntax-for type)]
                         [from (and/c identifier? (syntax-for type))]
                         [to (syntax-for type)]
                         [pred? (syntax-for predicate?)]
                         [fun (syntax-for (→ from acc (Values to acc)))])])]{
 Produces the syntax for the syntax for a function from the
 @racket[old-type] to the @racket[new-type], transforming
 all parts of the data structure which satisfy 
 @racket[pred?] using @racket[fun]. The @racket[new-type]
 will be the same as the one that would be returned by 
 @racket[(replace-in-type old-type #'([from to] …))]
 
 The generated function takes as a second argument an
 initial value for the accumulator. The accumulator is
 passed to @racket[fun] and the one returned is used as the
 accumulator for the next call. No guarantee is made on the
 order of traversal.
 
 @racket[pred?] should return true if and only if the data
 passed as an argument is an instance of the @racket[from]
 type. @racket[fun] should accept instances of the 
 @racket[from] type, and return instances of the 
 @racket[to] type.}
