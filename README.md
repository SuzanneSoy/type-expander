[![Build Status,](https://img.shields.io/travis/jsmaniac/type-expander/master.svg)](https://travis-ci.org/jsmaniac/type-expander)
[![Coverage Status,](https://img.shields.io/coveralls/jsmaniac/type-expander/master.svg)](https://coveralls.io/github/jsmaniac/type-expander)
[![Build Stats,](https://img.shields.io/website-stats-stats%20unavailable-blue-red/http/jsmaniac.github.io/travis-stats/.svg?label=build)](http://jsmaniac.github.io/travis-stats/#jsmaniac/type-expander)
[![Online Documentation,](https://img.shields.io/website-online-offline-blue-red/http/docs.racket-lang.org/type-expander/.svg?label=docs)](http://docs.racket-lang.org/type-expander/)
[![Maintained as of 2017.](https://img.shields.io/maintenance/yes/2017.svg)](https://github.com/jsmaniac/type-expander/issues)

Type-expander
=============

This project is written for
[Typed/Racket](https://docs.racket-lang.org/ts-guide/) using Literate
Programming.  See the “[Implementation of the type expander
library](http://docs.racket-lang.org/type-expander/)” part of the [online
documentation](http://docs.racket-lang.org/type-expander/) if you want to dig
into the source.

This library enhances typed/racket with type expanders, which are special
macros that can appear where a type would normally be expected, and must
expand to a type. Type expanders are to types what match expanders are to
match patterns. It is based on Asumu Takikawa's [type
expanders](https://github.com/racket/racket/compare/master...takikawa:tr-type-expander)
(see also his [original pull request
here](https://github.com/racket/racket/pull/604)).  Asumu Takikawa's work
attempted to integrate type expanders directly into Typed/Racket.  This
project instead implements type expanders as a library and works without any
modification of the core Typed/Racket codebase. This shows the extensibility
of Typed/Racket thanks to macros, and could serve as the basis for other
projects which need to alter the manner in which Typed/Racket handles types.

Installation
============

```
raco pkg install --deps search-auto type-expander
```

Usage example
=============

The `type-expander` is enabled by simply requiring the `type-expander` module
in a `typed/racket` program.

    #lang typed/racket
    (require type-expander)

For example, one can write the `(HomogeneousList n t)` type-expander, which
expands to the type for a list of `n` elements of type `t`:

    (require (for-syntax syntax/parse racket/list))
    (define-type-expander (HomogeneousList stx)
      (syntax-parse stx
        [(_ t:expr n:nat)
         #`(List #,@(map (λ (x) #'t)
                         (range (syntax-e #'n))))]))

It can then be used wherever a regular type is usually expected:

    (: five-strings (→ String (HomogeneousList String 5)))
    (define (five-strings x)
      (list x "a" "b" "c" "d"))
    
    (five-strings "hello")
    ;; => '("hello" "a" "b" "c" "d")
    
    (ann (five-strings "moon") (HomogeneousList String 5))
    ;; => '("moon"  "a" "b" "c" "d")
