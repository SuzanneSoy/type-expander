#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Variants}

@(table-of-contents)

@section{@racket[tagged]}

@section{@racket[define-tagged]}

@chunk[<define-tagged>
       (define-syntax/parse (define-tagged tag:id
                              (~maybe #:? tag?)
                              [field type] …)
         (define/with-syntax (pat …) (generate-temporaries #'(field …)))
         (define/with-syntax (value …) (generate-temporaries #'(field …)))
         (define/with-syntax default-tag? (format-id #'tag "~a?" #'tag))
         (template
          (begin
            (define-multi-id tag
              #:type-expand-once
              (tagged tag [field type] …)
              #:match-expander
              (λ/syntax-parse (_ pat …)
                #'(tagged tag [field pat] …))
              #:call
              (λ/syntax-parse (_ value …)
                #'(tagged tag #:instance [field value] …)))
            (: (?? tag? default-tag?) (→ Any Boolean))
            (define ((?? tag? default-tag?) x)
              (and (Tagged-predicate? tag x)
                   ((structure? field …) (Tagged-value x)))))))]

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

@section{Uninterned tags}

We wish to be able to declare tags only visible to the
creator, unlike the ones above which are visible
everywhere.

We will define two flavours: one where uninterned tags
inherit the interned tag, os that the interned @tc[tag] is a
supertype of the uninterned @tc[tag] (but not the converse),
and a second, which we will call private tags, where the
private tag inherits directly from @tc[Tagged], the base
structure described in section @secref{variant|supertype},
and is therefore unrelated to the interned @tc[tag] (nor to
the uninterned @tc[tag] either).

@; TODO: this should be integrated a bit better with other function, for example
@; Tagged-predicate? (provided as Tagged?) is likely to not work on uninterned
@; tags.

@chunk[<define-uninterned-tagged>
       (define-syntax/parse
           (define-private-tagged tag:id
             (~maybe #:? tag?)
             . (~and structure-type
                     ([field (~optional (~and C :colon)) type] …)))
         (define/with-syntax default-tag? (format-id #'tag "~a?" #'tag))
         (define-temp-ids "~a/struct" tag)
         (define-temp-ids "~a/arg" (field …))
         (define-temp-ids "~a/pat" (field …))
         (template
          (begin
            (struct (T) tag/struct Tagged ()) ; Private
            ;(struct (T) tag/struct interned ()) ; Uninterned
            (define-multi-id tag
              #:type-expand-once
              (tag/struct (structure . structure-type))
              #:match-expander
              (λ/syntax-parse (_ . (~and structure-pat
                                         ((~and field/pat :expr) …)))
                (quasitemplate
                 (and (? (make-predicate (tag/struct Any)))
                      (app Tagged-value
                           #,(syntax/loc #'structure-pat
                               (structure [field field/pat] …))))))
              #:call
              (λ/syntax-parse (_ . (~and args ((~and field/arg :expr) …)))
                (quasitemplate
                 (tag/struct #,(syntax/loc #'args
                                 (structure #:instance
                                            [field : type field/arg] …))))))
            ;; TODO: the return type is not precise enough, it should be:
            ;; #:+ (tag/struct (structure Any …))
            ;; #:- (! (tag/struct (structure Any …)))
            (: (?? tag? default-tag?) (→ Any Boolean :
                                         #:+ (tag/struct Any)))
            (define ((?? tag? default-tag?) x)
              (and ((make-predicate (tag/struct Any)) x)
                   ((structure? field …) (Tagged-value x)))))))]

@chunk[<define-uninterned-constructor>
       (define-syntax/parse
           (define-private-constructor tag:id
             (~maybe #:? tag?)
             T:expr …)
         (define/with-syntax default-tag? (format-id #'tag "~a?" #'tag))
         (define-temp-ids "~a/struct" tag)
         (define-temp-ids "~a/arg" (T …))
         (define-temp-ids "~a/pat" (T …))
         (define/syntax-parse (~or ([T₀:expr arg₀ pat₀])
                                   ([Tᵢ:expr argᵢ patᵢ] …))
           #'([T T/arg T/pat] …))
         (template
          (begin
            (struct (X) tag/struct Tagged ()) ; Private
            ;(struct (X) tag/struct interned ()) ; Uninterned
            (define-multi-id tag
              #:type-expand-once
              (tag/struct (?? T₀ (List Tᵢ …)))
              #:match-expander
              (λ/syntax-parse (_ . (~and pats (?? ((~and pat₀ :expr))
                                                  ((~and patᵢ :expr) …))))
                (quasitemplate
                 (and (? (make-predicate (tag/struct Any)))
                      (app Tagged-value
                           #,(syntax/loc #'pats
                               (?? pat₀ (list patᵢ …)))))))
              #:call
              (λ/syntax-parse (_ . (~and args (?? ((~and arg₀ :expr))
                                                  ((~and argᵢ :expr) …))))
                (quasitemplate
                 (tag/struct #,(syntax/loc #'args
                                 (?? arg₀ (list argᵢ …)))))))
            (: (?? tag? default-tag?) (→ Any Boolean : (tag/struct Any)))
            (define ((?? tag? default-tag?) x)
              ((make-predicate (tag/struct Any)) x)))))]

@chunk[<test-uninterned-tagged>
       (define-syntax-rule (defp make mt)
         (begin
           (define-private-tagged txyz #:? txyz?
             [a Number]
             [b String])
           
           (define (make) (txyz 1 "b"))
           
           (define (mt v)
             (match v
               ((txyz x y) (list 'macro y x))
               (_ #f)))))
       
       (defp make mt)
       
       (define-private-tagged txyz #:? txyz?
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

@chunk[<test-uninterned-constructor>
       (define-syntax-rule (defpc makec mtc)
         (begin
           (define-private-constructor cxyz #:? cxyz? Number String)
           
           (define (makec) (cxyz 1 "b"))
           
           (define (mtc v)
             (match v
               ((cxyz x y) (list 'macro y x))
               (_ #f)))))
       
       (defpc makec mtc)
       
       (define-private-constructor cxyz #:? cxyz? Number String)
       
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
                    "../lib/low.rkt"
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "structure.lp2.rkt")
           (provide (rename-out [Tagged-predicate? Tagged?]
                                [Tagged-type TaggedTop])
                    Tagged-value
                    constructor
                    define-variant
                    define-private-tagged
                    define-private-constructor)
           
           <variant-supertype>
           <remember-tags>
           <named-sorted-tags>
           <declare-all-tags>
           <tag-name→stx-name>
           
           <predicate>
           <constructor>
           <define-variant>
           <tagged>
           <define-tagged>
           <define-uninterned-tagged>
           <define-uninterned-constructor>
           
           (module+ test-helpers
             #;(provide Tagged-value)))
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    (submod ".." main test-helpers)
                    typed/rackunit
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           
           <test-constructor>
           <test-define-variant>
           <test-tagged>
           <test-define-tagged>
           <test-uninterned-tagged>
           <test-uninterned-constructor>))]
