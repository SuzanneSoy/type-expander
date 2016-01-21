[![Build Status,](https://img.shields.io/travis/jsmaniac/phc/master.svg)](https://travis-ci.org/jsmaniac/phc)
[![Coverage Status,](https://img.shields.io/coveralls/jsmaniac/phc/master.svg)](https://coveralls.io/github/jsmaniac/phc)
[![Online Documentation.](https://img.shields.io/badge/docs-online-brightgreen.svg)](http://jsmaniac.github.io/phc/)

This project is written in Typed/Racket using Literate Programming. See the
[online documentation](http://jsmaniac.github.io/phc/) if you want to dig into
the source.

How to build this project
=========================

To install the build dependencies of this project on a clean Ubuntu 14.04
machine, first install a recent [snapshot version of
 racket](https://pre.racket-lang.org/installers/). You will also need
to install `graphviz`. Then run the following commands:

    cd graph-lib/
    sudo apt-get install git
    git submodule init
    git submodule update
    make build-dep

To build the project, simply run `make`:

    make

List of files
=============

Graph library
-------------

* `graph/graph.lp2.rkt`

  Implements a graph construction library. The user specifies a series of
  transformations from a root input value to a result node, and can safely call
  other transformations, without risking to run into an infinite loop.

  The make-graph-constructor takes the types of the result nodes, and the
  transformations, and creates a graph-construction routine that detects cycles,
  and prevents infinite recursion.

  For now, in the result, calls to other transformations are replaced by link
  requests, but later versions will allow seamless access to the target result
  node.

  The make-graph-constructor macro can be used like this:

        (define make-g
          (make-graph-constructor
           ([ma (fav String) (faa ma) (fab mb)]
            [mb (fbv String) (fba ma)])
           [transform-a (s String) : ma
                        (ma s
                            (transform-a s)
                            (transform-b "b"))]
           [transform-b (s String) : mb
                        (mb s
                            (transform-a s))]))
      
  Then, when the resulting graph constructor is called:

        (make-g "root-arg")

  We get the following graph as a result:

        (ma "root-arg" <link-request1> <link-request2>)
        ^                   |               |
        `-------------------'        ,------'
                                     v
                                     (mb "b" <link-request3>)
                                     ^            |
        ,--------------------------- | -----------'
        v                            |
        (ma "b" <link-request4> <link-request5>)
        ^            |
        `------------'

* `graph/type-system.scrbl`
  
  Documentation about the type system implemented by `graph/graph2.lp2.rkt`,
  `graph/variant.lp2.rkt` and `graph/structure.lp2.rkt`.

* `graph/variant.lp2.rkt`
  
  Implements tagged unions, similar to those in caml, F#, or the unions in C if
  a tag field is added, with a distinct tag value for each type in the union.

* `graph/structure.lp2.rkt`
  
  Implements structures with anonymous access: accessing the field `b` of a
  structure instance `i` can be written: `(get i b)`, independently of the
  structure type of the instance. Traditionnally, if one creates two structs in
  Racket, using:
  
        (struct s1 ([a : Number] [b : String]))
        (struct s2 ([b : String] [c : Boolean]))
  
  then, to access the `b` field of an instance `i` of `s1`, one has to write
  `(s1-b i)`, whereas to access the field `b` of an instance `j` of `s2`, one
  has to write `(s2-b j)`. The fact that we need to know the type of an instance
  to access one of its fields is impractical when one needs to work with a large
  number of structs which share some field names.

  Inheritance could solve this problem, but Racket only has single inheritance
  for structs, not multiple inheritance.
  
  With the syntax offered by this library, one can write `(get i b)` for the
  first case and `(get j b)`, therefore allowing more flexible use of structures
  containing similar fields.
  
* `graph/equatable.rkt`
  
  Provides a struct in typed/racket with customizable equality, hashing and
  printing. This is similar to overriding `.equals()`, `.hashCode()` and
  `.toString()` in Java.

  Untyped racket provides these features out of the box, but they are not
  normally available in typed/racket.

* `graph/dotlang.rkt`
  
  Small programming language extension that allows writing `instance.f.g.h`
  instead of `(get (get (get instance f) g) h)`.

* `graph/remember.rkt`

  This utility module is used by `graph/structure.lp2.rkt` to memoize structure
  descriptiors. When the `structure` macro defined in `graph/structure.lp2.rkt`
  encounters an unknown list of field names, it adds it to the file
  `graph/remember.rkt`. The memoized descriptors are used to know all possible
  structs that can contain a field with the desired name when aceesing it with
  `(get instance field-name)`. The `get` macro can then dispatch on these types,
  and retrieve the field's value using the right accessor (for example
  `(s123-field-name instance)`).

* `graph/list-lang.rkt`

  Tiny programming language extension that allows constructing a list with the
  contents of all trailing lines in the file. This is used by
  `graph/remember.rkt` to easily add elements to the list of memoized structure
  descriptors, by just appending to the end of the file.

Type-expander
-------------

* `type-expander/type-expander.lp2.rkt`
  
  This library extends the type system to allow type-expander macros, much in
  the same way that `(match …)` allows match-expanders. For example, one can
  write the `(Repeat t n)` type-expander, which expands to a list of `n`
  elements of type `t`:

        (define-type-expander (Repeat stx)
                 (syntax-case stx ()
                   [(_ t n)
                    #`(List #,@(map (λ (x) #'t)
                                    (range (syntax->datum #'n))))]))

  It can then be used in places where a regular type is expected:

        (: count-five-more (→ Number (Repeat Number 5)))
        (define (count-five-more x)
          (list (+ x 1) (+ x 2) (+ x 3) (+ x 4) (+ x 5)))
        
        (count-five-more 3)
        ;; => '(4 5 6 7 8)
        
        (ann (count-five-more 15) (Repeat Number 5))
        ;; => '(16 17 18 19 20)

  This will be used by the graph library in `graph/graph.lp2.rkt` to allow
  expressing graph types anonymously:

        (: x-to-string (→ (graph (a [x Number] [b b])
                                 (b [a a] [y Boolean]))
                          (graph (a [x String] [b b])
                                 (b [a a] [y Boolean]))))
        (define (x-to-string g) …)

* `type-expander/multi-id.lp2.rkt`
  
  This library allows easy definition of an identifier with multiple semantics:
  the same identifier can be used as a constructor function, match expander,
  type, type-expander, expanded type (i.e. a type containing type-expanders,
  that has been expanded once), regular mutable or immutable variable, and with
  a custom write procedure.

  It allows defining a new type with its constructor and match expander with
  very little boilerplate code.

Library functions and utilities
-------------------------------

* `lib/eval-get-values.rkt`

  Wrapper for the racket `eval` function that allows evaluation of code with
  multiple return values in typed/racket.

* `lib/lib.rkt`
  
  Utilities that complement racket and typed/racket's standard libraries.

* `lib/low.rkt`

  Lower-level utilities that complement racket and typed/racket's standard
  libraries.

* `lib/low-untyped.rkt`
  
  Wrapper around `lib/low.rkt` that allows using it from a untyped racket file.

* `lib/untyped/for-star-list-star.rkt`

  A utility macro similar to `for*/list` to iterate over collections and return
  a list of results, but which can return nested lists instead of just a flat
  one.

* `lib/untyped.rkt`

  Aggregates `lib/low-untyped.rkt`, and `lib/untyped/for-star-list-star.rkt`.

* `lib/test-framework.rkt`
  
  Some wrappers and utilities that allow easier use of the rackunit test
  framework from typed/racket files.

* `lib/syntax/quasitemplate.rkt`

  Extension of the `syntax/parse/experimental/template` library, that allows
  using `unsyntax` and `unsyntax-splicing` inside a `quasitemplate`, just like
  in the normal `quasisyntax`.

* `lib/path.rkt`

  Filesystem path manipulation utilities.

* `lib/doc.rkt`

  Enhancements and utilities for documentation and literate programming files
  using scribble and scribble/lp2.

* `lib/doc/math.rkt`

  Allows typesetting mathematical formulas in documentation and literate
  programming files using scribble and scribble/lp2.

* `lib/doc/template.lp2.rkt`

  Example document using the features in `lib/doc.rkt` and `lib/doc/math.rkt`.

* `lib/doc/example.lp2.rkt`

  Other example document using the features in `lib/doc.rkt` and
  `lib/doc/math.rkt`.

Makefile
--------

* `make/make.rkt`

  This program acts like a Makefile, it is used to compile the rest of
  the code.

* `make/lib.rkt`

  Function definitions used by the `make/make.rkt` tool.
