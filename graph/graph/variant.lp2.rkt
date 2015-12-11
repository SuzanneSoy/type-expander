#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Variants}

@(table-of-contents)

@section{Introduction}

We define variants (tagged unions), with the following constraints:

@itemlist[
 @item{Unions are anonymous: two different unions can contain the same tag, and
  there's no way to distinguish these two occurrences of the tag}
 @item{The tag can be followed by zero or more “fields”}
 @item{An instance of a variant only @racket[match]es with its constructor and
  the same number of fields}]

See @url{https://github.com/andmkent/datatype/} for an existing module providing
Algebraic Data Types.

@section{@racket[constructor]}

We define the variant as a @tc[list], with the tag symbol in the first element.
We can't use a @tc[vector], because these are mutable in @tc[typed/racket], and
occurrence typing can't work properly on mutable data structures (yet).

@chunk[<constructor>
       (define-multi-id constructor
         #:type-expander <type-expander>
         #:match-expander <match-expander>
         #:call <make-instance>)]

@chunk[<test-constructor>
       (check-equal? (ann (constructor a 1 "x")
                          (constructor a Number String))
                     (list 'a 1 "x"))
       (check-equal? (ann (constructor b)
                          (constructor b))
                     (list 'b))
       (check-equal? (ann (constructor c 2 "y")
                          (constructor c Number String))
                     (constructor c 2 "y"))
       (check-not-equal? (constructor d 2 "y")
                         (constructor d 2 "y" 'z))
       (check-not-equal? (constructor e 2 "y")
                         (constructor F 2 "y"))]

@subsection{Type-expander}

@chunk[<type-expander>
       (λ/syntax-parse (_ tag:id type:expr ...)
         #'(List 'tag type ...))]

@subsection{Match-expander}

@chunk[<match-expander>
       (λ/syntax-parse (_ tag:id pat:expr ...)
         #'(list 'tag pat ...))]

@subsection{Actual constructor}

@chunk[<make-instance>
       (λ/syntax-parse (_ tag:id value:expr ...)
         (define/with-syntax (arg ...) (generate-temporaries #'(value ...)))
         (define/with-syntax (T ...) (generate-temporaries #'(value ...)))
         #'((λ #:∀ (T ...) ([arg : T] ...) : (List 'tag T ...)
              (list 'tag arg ...))
            value ...))]

@section{@racket[define-variant]}

In @tc[define-variant], we only define the type (which is the union of all the
possible constructors. We don't define the constructors, for two reasons: the
same @tc[constructor]s could appear in several variants, so we would define them
twice, and it is likely that a constructor will have the same identifier as an
existing variable or function.

@chunk[<define-variant>
       (define-syntax/parse (define-variant name [tag:id type:expr ...] ...)
         #'(define-type name (U (constructor tag type ...) ...)))]

@chunk[<test-define-variant>
       (define-variant v1 [x Number String] [y String Number] [z Number String])
       (check-equal? (ann (constructor x 1 "a")
                          (U [constructor w Number String]
                             [constructor x Number String]
                             [constructor y String Number]))
                     (constructor x 1 "a"))
       (check-equal? (constructor x 1 "a")
                     (constructor x 1 "a"))
       (check-equal? (ann (constructor x 1 "a") v1)
                     (constructor x 1 "a"))
       (check-equal? (ann (constructor x 1 "a") v1)
                     (ann (constructor x 1 "a") v1))
       (check-not-equal? (ann (constructor x 2 "b") v1)
                         (ann (constructor y "b" 2) v1))
       (check-not-equal? (ann (constructor x 3 "c") v1)
                         (ann (constructor z 3 "c") v1))]

This makes pattern-matching more verbose, though, since we have to specify
@tc[(variant tag pat ...)] each time, instead of just @tc[(tag pat ...)]. I
don't really know how to solve that. It should be noted that constructors are
likely to have names starting with a capital letter, so maybe this reduces the
number of name collisions.

@section{@racket[tagged]}

@CHUNK[<tagged>
       (define-multi-id tagged
         #:type-expander
         (λ/syntax-parse (_ tag:id . structure-type)
           #`(constructor tag #,(syntax/loc #'structure-type
                                  (structure . structure-type))))
         #:match-expander
         (λ/syntax-parse (_ tag:id . structure-pat)
           #`(constructor tag #,(syntax/loc #'structure-pat
                                  (structure . structure-pat))))
         #:call ;; TODO: clean this up a bit, and explain it.
         (λ/syntax-parse
           (~and (_ (~and (~seq disambiguate …) (~or (~seq #:instance)
                                                     (~seq #:constructor)
                                                     (~seq)))
                    tag:id . fields)
                 (~parse (sa:structure-args-stx-class)
                           #'(disambiguate … . fields)))
           (define-temp-ids "~a/TTemp" (sa.field …))
           (define/with-syntax c
             (if (attribute sa.type)
                 #`(λ ([sa.field : sa.type] …)
                     : (constructor tag #,(syntax/loc #'fields
                                            (structure [sa.field sa.type] …)))
                     (constructor tag
                                  #,(syntax/loc #'fields
                                      (structure #:instance
                                                 [sa.field : sa.type sa.field] …))))
                 #`(λ #:∀ (sa.field/TTemp …) ([sa.field : sa.field/TTemp] …)
                     : (constructor tag #,(syntax/loc #'fields
                                            (structure [sa.field sa.field/TTemp] …)))
                     (constructor tag
                                  #,(syntax/loc #'fields
                                      (structure #:instance
                                                 [sa.field sa.field] …))))))
           (if (attribute sa.value)
               #'(c sa.value …)
               #'c)))]

@chunk[<test-tagged>
       (check-equal? (match (ann (tagged foo [x "o"] [y 3] [z 'z])
                                 (tagged foo
                                         [x String]
                                         [z 'z]
                                         [y Fixnum]))
                       [(tagged foo z x y) (list z y x)])
                     '(z 3 "o"))]

@section{@racket[define-tagged]}

@chunk[<define-tagged>
       (define-syntax/parse (define-tagged tag:id [field type] ...
                              (~optional #:type-noexpand))
         (define/with-syntax (pat ...) (generate-temporaries #'(field ...)))
         (define/with-syntax (value ...) (generate-temporaries #'(field ...)))
         #'(define-multi-id tag
             #:type-expand-once
             (tagged tag [field type] ...)
             #:match-expander
             (λ/syntax-parse (_ pat ...)
               #'(tagged tag [field pat] ...))
             #:call
             (λ/syntax-parse (_ value ...)
               #'(tagged tag #:instance [field value] ...))))]

@chunk[<test-define-tagged>
       (define-tagged tagged-s1)
       (define-tagged tagged-s2 [f Fixnum] [g String])
       (define-tagged tagged-s3 [g String] [f Fixnum])
       (define-tagged tagged-s4 [f Fixnum] [g String])
       
       (check-equal? (match (ann (tagged-s1) (tagged tagged-s1))
                       [(tagged-s1) #t])
                     #t)
       
       (check-equal? (match (ann (tagged-s2 99 "z") tagged-s2)
                       [(tagged-s2 f g) (cons g f)])
                     '("z" . 99))
       
       (let ()
         (check-equal? (match (ann (tagged-s2 99 "in-let") tagged-s2)
                         [(tagged-s2 f g) (cons g f)])
                       '("in-let" . 99)))
       
       (define (test-match val)
         (match val
           [(tagged-s2 x y) (list 'found-s2 y x)]
           [(tagged-s3 x y) (list 'found-s3 y x)]
           [(tagged-s4 x y) (list 'found-s4 y x)]))
       
       (check-equal?
        (test-match (ann (tagged-s2 2 "flob")
                         (tagged tagged-s2 [f Fixnum] [g String])))
        '(found-s2 "flob" 2))
       
       (check-equal?
        (test-match (ann (tagged-s3 "flob" 2)
                         (tagged tagged-s3 [g String] [f Fixnum])))
        '(found-s3 2 "flob"))
       
       ;; g and f are inverted in the “ann”
       (check-equal?
        (test-match (ann (tagged-s4 2 "flob")
                         (tagged tagged-s4 [g String] [f Fixnum])))
        '(found-s4 "flob" 2))
       
       (define (test-match-verbose val)
         (match val
           [(tagged tagged-s2 g [f y]) (list 'found-s2 g y)]
           [(tagged tagged-s3 [g y] f) (list 'found-s2 f y)]
           [(tagged tagged-s4 [f y] g) (list 'found-s2 g y)]))
       
       (check-equal?
        (test-match (ann (tagged-s2 3 "flob")
                         (tagged tagged-s2 [f Fixnum] [g String])))
        '(found-s2 "flob" 3))
       
       ;; g and f are inverted in the “ann”
       (check-equal?
        (test-match (ann (tagged-s3 "flob" 3)
                         (tagged tagged-s3 [f Fixnum] [g String])))
        '(found-s3 3 "flob"))
       
       (check-equal?
        (test-match (ann (tagged-s4 3 "flob")
                         (tagged tagged-s4 [f Fixnum] [g String])))
        '(found-s4 "flob" 3))
       
       (check-not-equal? (tagged-s2 4 "flob")
                         (tagged-s3 "flob" 4))
       (check-not-equal? (tagged-s2 4 "flob")
                         (tagged-s4 4 "flob"))]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax syntax/parse
                                racket/syntax
                                "../lib/low-untyped.rkt")
                    "../lib/low.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "structure.lp2.rkt")
           (provide constructor
                    define-variant
                    tagged
                    define-tagged)
           
           <constructor>
           <define-variant>
           <tagged>
           <define-tagged>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    typed/rackunit
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           <test-constructor>
           <test-define-variant>
           <test-tagged>
           <test-define-tagged>
           
           (require (submod ".." doc))))]