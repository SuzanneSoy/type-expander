#lang scribble/manual

@(require (for-label typed/racket/base
                     "adt.lp2.rkt")
          "../lib/doc.rkt")
@doc-lib-setup

@defform[#:kind "type expander"
         #:literals (:)
         (tagged tag field-desc)
         #:grammar
         [(tag Identifier)
          (field-desc field
                      [field]
                      [field : type])
          (field Identifier)
          (type Type)]]{
 Expands to the type for a tagged structure with the given
 type and fields. If the types for the fields are not given,
 a polymorphic structure is returned.
 
 For now, either all types must be given or none, as
 keeping track of which fields are polymorphic and which are
 not would be difficult for the user. In the rare cases
 whrere partially polymorphic fields would be needed, the
 type can easily be described using:
 
 @racketblock[
 (define-type (partially-polymorphic A C)
   (tagged some-tag [a A] [b String] [c C] [d Integer]))]}

@defform[#:kind "match expander"
         (tagged)]{
}

@defform*[#:literals (:)
          ((tagged maybe-instance tag just-field …)
           (tagged maybe-make-instance tag field+value …))
          #:grammar
          [(maybe-instance (code:line)
                           #:instance)
           (maybe-make-instance (code:line)
                                #:make-instance)
           (tag Identifier)
           (just-field field
                       [field]
                       [field : type])
           (field+value [field value]
                        [field : type value])
           (field Identifier)
           (type Type)
           (value Expression)]]{
 When using the @racket[just-field] syntax, this form
 produces a function which can be used to build an instance
 of the tagged structure, by passing as many values as there
 are fields.
 
 When using the @racket[field+value] syntax, this form
 directly returns an instance of the tagged structure, with
 the given values.
 
 It is mandatory to disambiguate with either 
 @racket[#:instance] or @racket[#:make-instance] when using
 @racket[tagged] with an empty list of fields (i.e. a
 structure with no fields) as it cannot be guessed from the
 syntax otherwise, so it is best to always include it when
 producing a call to @racket[tagged] in a macro.}

@defform[(tagged? tag field …)
         #:contracts ([tag Identifier]
                      [field Identifier])]{
 Returns a predicate for tagged structures with the given 
 @racket[tag] and @racket[field]s.}

@defform[(tagged? tag
                  #:with-struct with-struct
                  field)
         #:contracts ([with-struct struct-type?])]{
 The @racket[#:with-struct] option is reserved for internal
 use. It is used by @racket[#:private] and 
 @racket[#:uninterned] in @racket[define-contructor] and 
 @racket[define-tagged].}

@defidform[#:kind "type"
           TaggedTop]{
 The supertype of all tagged structures.}

@defproc[(TaggedTop?) Boolean]{
 Predicate for tagged structures. It will also return 
 @racket[#t] for constructors which only value is a promise
 @note{The reason why we wrap the structure with a promise
  is to allow building circular data structures, like what
  is done in the @racket[graph] library. The structure is
  always wrapped in a promise even for non-cyclic data
  structures, so that the @racket[get] operation always
  operates on the same type. Although this incurs an extra
  cost in terms of memory usage, it avoids combinatorial
  blowup of the size of the type of an object which has the
  nested fields @racket[.f₁.f₂.f₃.….fₙ], which would
  otherwise have size @${2ⁿ}, due to the presence of a 
  @racket[(U (Promise …) …)] at each level.} for a
 structure, because this is how tagged structures are
 implemented behind the scenes.}

@defform[(define-constructor name maybe-private-uninterned maybe-? type …)
         #:grammar
         [(maybe-private-uninterned (code:line)
                                    #:private
                                    #:uninterned)
          (maybe-? (code:line)
                   (code:line #:? name?))
          (name Identifier)
          (name? Identifier)
          (type Type)]]{
 Binds @racket[name] to the constructor's type-expander,
 match-expander and creation function. @racket[name?] is
 bound to the predicate for that constructor.

 Unless specified, @racket[name?] is derived from 
 @racket[name] by appending @racket[?] to the identifier.

 @racket[#:private] and @racket[#:uninterned] allow
 specifying a constructor which is not visible from other
 files or declarations. @racket[#:uninterned] constructors
 are subtypes of the default, interened ones, while 
 @racket[#:private] are directly subtypes of ConstructorTop,
 meaning that they won't be matched by a "public"
 constructor with the same name.}
