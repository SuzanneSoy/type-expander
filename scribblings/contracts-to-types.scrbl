#lang scribble/manual

@require[(for-label racket/contract/base)
         scribble/example]
@title{Using contract syntax to specify types}

@defmodule[type-expander/contracts-to-types]

@defform*[{(contract→type contract)
           (contract->type contract)}]{

 This is a simple type expander which translates common contracts to types.
 Note that it only supports a limited number of contract constructors. The
 following are supported: @racket[or/c], @racket[and/c] (the translation may
 produce a type too complex for Typed/Racket to understand properly, though),
 @racket[listof], @racket[list/c], @racket[*list/c], @racket[vectorof],
 @racket[vector/c], @racket[cons/c], @racket[integer?], @racket[string?],
 @racket[symbol?], @racket[exact-nonnegative-integer?],
 @racket[exact-positive-integer?], @racket['quoted-datum],
 @racket[`quasiquoted-datum-with-unquoted-types].

 Furthermore, using @racket[,_τ] anywhere outside of a quoted datum will leave
 the type @racket[_τ] unchaged, allowing the user to manually convert to types
 only the parts which cannot be converted automatically.}

@defform*[{(:contract→type contract)
           (:contract->type contract)}]{
                                        
 Prints a representation of the contract translated as a type. It is then
 possible to copy-paste that result into the code.
                                        
 @examples[
 (require type-expander/lang
          racket/contract/base
          type-expander/contracts-to-types)
 (:contract→type (list/c 1 2 "str" (or/c integer? string?)))]
}
