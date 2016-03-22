#lang scribble/manual

@(require (for-label typed/racket/base
                     "rewrite-type.lp2.rkt"))

@title{Rewrite-type utilities for writing type-level functions}

The utilities described here allow replacing parts of a
type with other types, and generating conversion functions
transforming instances of the old type into instances of the
new type.

@defform[#:kind "template metafunction"
         (tmpl-fold-instance old-type
           accumulator-type
           [from to pred? fun] …)
         #:contracts ([old-type type]
                      [accumulator-type type]
                      [from identifier?]
                      [to type]
                      [pred? predicate?]
                      [fun (→ from acc (values to acc))])]{
 Produces the syntax for a function from @racket[old-type]
 to the new type, using the provided replacement functions
 for each part.}


@defform[#:kind "template metafunction"
         (tmpl-replace-in-instance old-type
                                   [from to pred? fun] …)
         #:contracts ([old-type type]
                      [accumulator-type type]
                      [from identifier?]
                      [to type]
                      [pred? predicate?]
                      [fun (→ from to)])]{
 Produces the syntax for a function from @racket[old-type]
 to the new type, using the provided replacement functions
 for each part.}

@defform[#:kind "procedure"
         (replace-in-type old-type #'([from to] …))
         #:contracts ([old-type type]
                      [from identifier?]
                      [to type])]{
 This type-level function produces the syntax for the type 
 @racket[old-type], with all occurrences of @racket[from]
 replaced with @racket[to] in the type.}

@defform[#:kind "procedure"
         (replace-in-instance old-type #'([from to pred? fun] …))
         #:contracts ([old-type type]
                      [from identifier?]
                      [to type]
                      [pred? predicate?]
                      [fun (→ from to)])]{
 Produces the syntax for the syntax for a function from 
 @racket[old-type] to the new type, transforming all parts
 of the data structure which satisfy @racket[pred?] using 
 @racket[fun]. @racket[pred?] should return true if and only
 if the data pased as an argument is an instance of the 
 @racket[from] type. @racket[fun] should accept instances of
 the @racket[from] type, and return instances of the 
 @racket[to] type.}