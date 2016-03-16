#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: 
 @racket[define-constructor] and @racket[define-tagged]}

@(table-of-contents)

@section{Introduction}

@section{@racket[uninterned] and @racket[private]}

We wish to be able to declare tags and constructors only
visible to the creator, unlike the default ones which can be
instantiated and matched against anonymously.

We will define two flavours. In the first case, 
@tc[uninterned] constructors inherit the interned one. It
means that the interned constructor is a supertype of the
uninterned constructor (but not the converse). Two distinct
uninterned constructors with the same name are unrelated
too. The second possibility is to declare a @tc[private]
constructor, where the private constructor inherits directly
from @tc[ConstructorTop], the base structure described in
section @secref{constructor|supertype}, and is therefore
unrelated to the interned constructor (and is unrelated to
the uninterned constructor too).

The choice to declare an uninterned or private 

@CHUNK[<uninterned+private>
       (define/syntax-parse ((~maybe with-struct declare-uninterned/private))
         (cond [(attribute uninterned)
                #`(#,(syntax-local-introduce #'constructor-name)
                   declare-uninterned-constructor-struct)]
               [(attribute private)
                #`(#,(syntax-local-introduce #'constructor-name)
                   declare-constructor-struct)]
               [else #'()]))]

The above code binds @tc[declare-uninterned/private] to
either @tc[declare-uninterned-constructor-struct] or 
@tc[declare-constructor-struct], depending on the keyword
used. The macro's expansion will use this to declare 
@tc[with-struct].

@chunk[<declare-uninterned-or-private-struct>
       (?? (declare-uninterned/private with-struct))]

@section{@racket{define-constructor}}

@chunk[<define-constructor>
       (define-syntax/parse
           (define-constructor constructor-name:id
             (~maybe (~optkw #:uninterned) (~optkw #:private))
             (~maybe #:? name?)
             type …)
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define-temp-ids "pat" (type …))
         (define-temp-ids "value" (type …))
         <uninterned+private>
         (template
          (begin
            <declare-uninterned-or-private-struct>
            (define-multi-id constructor-name
              #:type-expand-once
              (constructor constructor-name
                           (?? (?@ #:with-struct with-struct))
                           type …)
              #:match-expander
              (λ/syntax-parse (_ pat …)
                #'(constructor constructor-name
                               (?? (?@ #:with-struct with-struct))
                               pat …))
              #:call
              (λ/syntax-parse (_ value …)
                #'(constructor constructor-name
                               (?? (?@ #:with-struct with-struct))
                               value …)))
            (define-multi-id (?? name? default-name?)
              #:else
              #'(constructor? constructor-name
                              (?? (?@ #:with-struct with-struct)))))))]

@chunk[<define-tagged>
       (define-syntax/parse (define-tagged constructor-name:id
                              (~maybe (~optkw #:uninterned) (~optkw #:private))
                              (~maybe #:? name?)
                              [field type] …)
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define-temp-ids "pat" (type …))
         (define-temp-ids "value" (type …))
         <uninterned+private>
         (template
          (begin
            <declare-uninterned-or-private-struct>
            (define-multi-id constructor-name
              #:type-expand-once
              (tagged constructor-name
                      (?? (?@ #:with-struct with-struct))
                      [field type] …)
              #:match-expander
              (λ/syntax-parse (_ pat …)
                #'(tagged constructor-name
                          (?? (?@ #:with-struct with-struct))
                          [field pat] …))
              #:call
              (λ/syntax-parse (_ value …)
                #'(tagged #:instance
                          constructor-name
                          (?? (?@ #:with-struct with-struct))
                          [field value] …)))
            (define-multi-id (?? name? default-name?)
              #:else
              #'(tagged? constructor-name
                         (?? (?@ #:with-struct with-struct))
                         field …)))))]

@section{Tests}

@chunk[<test-define-tagged>
       (define-tagged tagged-s1)
       (define-tagged tagged-s2 [f Fixnum] [g String])
       (define-tagged tagged-s3 [g String] [f Fixnum])
       (define-tagged tagged-s4 [f Fixnum] [g String])
       
       (check-equal?: (match (ann (tagged-s1) (tagged tagged-s1))
                        [(tagged-s1) #t])
                      #t)
       
       (check-equal?: (match (ann (tagged-s2 99 "z") tagged-s2)
                        [(tagged-s2 f g) (cons g f)])
                      '("z" . 99))
       
       (let ()
         (check-equal?: (match (ann (tagged-s2 99 "in-let") tagged-s2)
                          [(tagged-s2 f g) (cons g f)])
                        '("in-let" . 99)))
       
       (define (test-match val)
         (match val
           [(tagged-s2 x y) (list 'found-s2 y x)]
           [(tagged-s3 x y) (list 'found-s3 y x)]
           [(tagged-s4 x y) (list 'found-s4 y x)]))
       
       (check-equal?:
        (test-match (ann (tagged-s2 2 "flob")
                         (tagged tagged-s2 [f Fixnum] [g String])))
        '(found-s2 "flob" 2))
       
       (check-equal?:
        (test-match (ann (tagged-s3 "flob" 2)
                         (tagged tagged-s3 [g String] [f Fixnum])))
        '(found-s3 2 "flob"))
       
       ;; g and f are inverted in the “ann”
       (check-equal?:
        (test-match (ann (tagged-s4 2 "flob")
                         (tagged tagged-s4 [g String] [f Fixnum])))
        '(found-s4 "flob" 2))
       
       (define (test-match-verbose val)
         (match val
           [(tagged tagged-s2 g [f y]) (list 'found-s2 g y)]
           [(tagged tagged-s3 [g y] f) (list 'found-s2 f y)]
           [(tagged tagged-s4 [f y] g) (list 'found-s2 g y)]))
       
       (check-equal?:
        (test-match (ann (tagged-s2 3 "flob")
                         (tagged tagged-s2 [f Fixnum] [g String])))
        '(found-s2 "flob" 3))
       
       ;; g and f are inverted in the “ann”
       (check-equal?:
        (test-match (ann (tagged-s3 "flob" 3)
                         (tagged tagged-s3 [f Fixnum] [g String])))
        '(found-s3 3 "flob"))
       
       (check-equal?:
        (test-match (ann (tagged-s4 3 "flob")
                         (tagged tagged-s4 [f Fixnum] [g String])))
        '(found-s4 "flob" 3))
       
       (check-not-equal?: (tagged-s2 4 "flob")
                          (tagged-s3 "flob" 4))
       (check-not-equal?: (tagged-s2 4 "flob")
                          (tagged-s4 4 "flob"))]

@chunk[<test-define-constructor>
       (define-constructor c1)
       (define-constructor c2 Fixnum String)
       (define-constructor c3 Fixnum String)
       
       (check-equal?: (match (ann (c1) (constructor c1))
                        [(c1) #t])
                      #t)
       
       (check-equal?: (match (ann (c2 99 "z") c2)
                        [(c2 f g) (cons g f)])
                      '("z" . 99))
       
       (let ()
         (check-equal?: (match (ann (c2 99 "in-let") c2)
                          [(c2 f g) (cons g f)])
                        '("in-let" . 99)))
       
       (define (test-c-match val)
         (match val
           [(c1) (list 'found-c1)]
           [(constructor c2 x y z) (list 'found-c2-xyz z y x)]
           [(c2 x y) (list 'found-c2 y x)]
           [(c3 x y) (list 'found-c3 y x)]))
       
       (check-equal?:
        (test-c-match (ann (c2 2 "flob")
                           (constructor c2 Fixnum String)))
        '(found-c2 "flob" 2))
       
       (check-equal?:
        (test-c-match (ann (c3 2 "flob")
                           (constructor c3 Fixnum String)))
        '(found-c3 "flob" 2))]

@chunk[<test-private-tagged>
       (define-syntax-rule (defp make mt)
         (begin
           (define-tagged txyz #:private #:? txyz?
             [a Number]
             [b String])
           
           (define (make) (txyz 1 "b"))
           
           (define (mt v)
             (match v
               ((txyz x y) (list 'macro y x))
               (_ #f)))))

       (defp make mt)
       
       (define-tagged txyz #:private #:? txyz?
         [a Number]
         [b String])
       
       (check-equal?: (match (make)
                        ((tagged txyz x y) (list 'out y x))
                        (_ #f))
                      #f)
       
       (check-equal?: (mt (tagged txyz [x 1] [y "b"]))
                      #f)
       
       (check-equal?: (mt (make))
                      '(macro "b" 1))
       
       (check-not-equal?: (make) (txyz 1 "b"))
       (check-equal?: (match (make)
                        ((txyz x y) (list 'out y x))
                        (_ #f))
                      #f)
       
       (check-equal?: (mt (txyz 1 "b"))
                      #f)]

@chunk[<test-private-constructor>
       (define-syntax-rule (defpc makec mtc)
         (begin
           (define-constructor cxyz #:private #:? cxyz? Number String)
           
           (define (makec) (cxyz 1 "b"))
           
           (define (mtc v)
             (match v
               ((cxyz x y) (list 'macro y x))
               (_ #f)))))
       
       (defpc makec mtc)

       (define-constructor cxyz #:private #:? cxyz? Number String)
       
       (check-equal?: (match (makec)
                        ((constructor cxyz e f) (list 'out f e))
                        (_ #f))
                      #f)
       
       (check-equal?: (mtc (constructor cxyz 1 "b"))
                      #f)
       
       (check-equal?: (mtc (makec))
                      '(macro "b" 1))
       
       (check-not-equal?: (makec) (cxyz 1 "b"))
       (check-equal?: (match (makec)
                        ((cxyz e f) (list 'out f e))
                        (_ #f))
                      #f)
       
       (check-equal?: (mtc (cxyz 1 "b"))
                      #f)]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket/list
                                syntax/parse
                                syntax/parse/experimental/template
                                racket/syntax
                                (submod "../lib/low.rkt" untyped))
                    (for-meta 2 racket/base)
                    "constructor.lp2.rkt"
                    (submod "constructor.lp2.rkt" main private)
                    "tagged.lp2.rkt"
                    "../lib/low.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           (provide define-constructor
                    define-tagged)
           
           <define-constructor>
           <define-tagged>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    "constructor.lp2.rkt"
                    "tagged.lp2.rkt"
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           <test-define-constructor>
           <test-define-tagged>
           <test-private-constructor>
           <test-private-tagged>))]