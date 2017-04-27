#lang scribble/manual

@require[@for-label[type-expander]]

@(module m racket/base
   (require scribble/manual)
   (provide e:colon)
   (require (for-label type-expander/expander))
   (define e:colon (racket colon)))
@(require 'm)

@title{Deprecated export of @racket[colon] via @racketmodname[type-expander]}

@declare-exporting[type-expander]

@defidform[colon]{
 @deprecated[
 #:what "reprovide"
 @list{@e:colon from @racketmodname[type-expander/expander]}
 @list{The @e:colon identifier is re-exported for-syntax as @racket[colon] by
   @racketmodname[type-expander]. Prefer instead explicitly using
   @racket[(require (for-syntax #,(racketmodname type-expander/expander)))], as
   the re-export will be removed in future versions.}]}