#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Overview of the implementation choices for structures, graphs and passes}

@;(table-of-contents)

@section[#:tag "type-system|structures"]{Structures}

@;(+ 1 2 3) gives @(format "~a" (+ 1 2 3)).

@;@(require racket/string racket/list)
@;@(string-join (map (λ (x) (format "~a bottles of beer on the wall, take one down and pass it around, ~a bottles of beer on the wall" x (- x 1)))
@;                   (reverse (range 1 99)))
@;              "\n")

Structures are represented as lists of key/value pairs.
@note{We need lists and can't use vectors (or hash tables) because the latter are mutable in @code|{typed/racket}|,
 and the typing system has no guarantee that accessing the same element twice will yield the same value (so occurence typing can't narrow the type in branches of conditionnals).}
@note{Actually, we can use structs (they are immutable by default, and the occurrence typing knows that). There are two problems with them:
 we can't have subtyping (although knowing all the structs used in the program means we can just filter them and use a @code{(U S1 S2 …)},
 and to declare the structs, we would need to be in a define-like environment (therefore making anonymous structure types problematic), although since we know all the structs in advance, we can pre-declare them in a shared file.}

@chunk[<example-simple-structure>
       (define-type abc (List (Pairof 'a Number)
                              (Pairof 'b String)
                              (Pairof 'c (U 'x 'y))))
       
       (: make-abc (→ Number String (U 'x 'y) abc))
       (define (make-abc a b c)
         (list (cons 'a a) (cons 'b b) (cons 'c c)))
       (make-abc 1 "b" 'x)]

Occurrence typing works:

@chunk[<example-simple-structure-occurrence>
       (: f (→ abc (U 'x #f)))
       (define (f v)
         (if (eq? 'x (cdr (cddr v)))
             (cdr (cddr v))
             #f))]

@section{Passes, subtyping and tests}


Below is the definition of a function which works on @tc[(structure [a Number] [b String] [c Boolean])], and returns the same structure extended with a field @tc[[d Number]],
but only cares about fields @tc[a] and @tc[c], so tests don't need to provide a value for @tc[b].

@chunk[<example-pass-which-extends-input>
       (: pass-calc-d (∀ (TB) (→ (List (Pairof 'a Number)
                                       (Pairof 'b TB)
                                       (Pairof 'c Boolean))
                                 (List (Pairof 'a Number)
                                       (Pairof 'b TB)
                                       (Pairof 'c Boolean)
                                       (Pairof 'd Number)))))
       (define (pass-calc-d v)
         (list (car v) ; a
               (cadr v) ; b
               (caddr v) ; c
               (cons 'd (+ (cdar v) (if (cdaddr v) 0 1)))))]

The example above can be called to test it with a dummy value for @tc[b]:

@chunk[<example-pass-which-extends-input>
       (pass-calc-d '((a . 1) (b . no-field) (c . #t)))]

But when called with a propper value for @tc[b], we get back the original string as expected, and the type is correct:

@chunk[<example-pass-which-extends-input>
       (ann (pass-calc-d '((a . 1) (b . "some string") (c . #t)))
            (List (Pairof 'a Number)
                  (Pairof 'b String)
                  (Pairof 'c Boolean)
                  (Pairof 'd Number)))]

If the pass should be able to work on multiple graph types (with more or less info), then it should be easy to mark it as a @tc[case→] function.
It's probably better to avoid too permissive subtyping, otherwise, imagine we have a pass which removes @tc[Addition]s and @tc[Substraction]s from an AST,
and replaces them with a single @tc[Arithmetic] node type. If we have full duck typing, we could call it with @tc[Addition]s and @tc[Substraction] hidden in fields
it does not know about, and so it would fail to replace them. Also, it could be called with an already-processed AST which already contains just @tc[Arithmetic] node types,
which would be a bug most likely. Therefore, explicitly specifying the graph type on which the passes work seems a good practice. Some parts
can be collapsed easily into a @tc[∀] type @tc[T], when we're sure there shouldn't be anything that interests us there.

@section{Graphs}

In order to be able to have cycles, while preserving the benefits of occurrence typing, we need to make sure that from the type system's point of view, accessing a successor node twice will return the same value each time.

The easiest way is to wrap the to-be-created value inside a @tc[Promise]. Occurrence typing works on those:

@chunk[<test-promise-occurence-typing>
       (: test-promise-occurence (→ (Promise (U 'a 'b)) (U 'a #f)))
       (define (test-promise-occurence p)
         (if (eq? (force p) 'a)
             (force p)
             #f))]

@section{Conclusion}

@chunk[<*>
       (module main typed/racket
         <example-simple-structure>
         <example-simple-structure-occurrence>
         
         <example-pass-which-extends-input>
         
         <test-promise-occurence-typing>)]
