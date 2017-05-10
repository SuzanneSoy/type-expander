#lang info
(define collection "type-expander")
(define deps '("base"
               "rackunit-lib"
               "scribble-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "hyper-literate"
               "auto-syntax-e"
               "debug-scopes"
               "version-case"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-more"
                     "typed-racket-doc"
                     "scribble-enhanced"
                     ;; Just for a link to the library inside the documentation.
                     ;; Can be removed and change the link
                     ;; in type-expander.hl.rkt to
                     ;; http://docs.racket-lang.org/mutable-match-lambda/
                     "mutable-match-lambda"))
(define scribblings '(("scribblings/type-expander.scrbl" () ("typed-racket"))
                      ("scribblings/type-expander-implementation.scrbl" (multi-page) ("typed-racket"))))
(define pkg-desc "Description Here")
(define version "1.0")
(define pkg-authors '(|Georges Dupéron|))
