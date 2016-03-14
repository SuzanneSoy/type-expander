#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: Tagged}

@(table-of-contents)

@section{Introduction}

We define @tc[tagged], which is a shorthand for
manipulating constructors which single value is a promise
for a structure.

@section{@racket[tagged]}

@chunk[<tagged>
       (define-multi-id tagged
         #:type-expander <type-expander>
         #:match-expander <match-expander>
         #:call <make-instance>)]

@subsection{@racket[TaggedTop]}

@chunk[<tagged-top>
       (define-type TaggedTop (ConstructorTop (Promise StructureTop)))]

@subsection{@racket[type-expander]}

@chunk[<type-expander>
       (λ/syntax-parse (_ tag:id (~maybe #:with-struct with-struct)
                          . structure-type)
         (quasitemplate
          (constructor tag (?? (?@ #:with-struct with-struct))
                       #,(syntax/loc #'structure-type
                           (structure . structure-type)))))]

@subsection{@racket[match-expander]}

@chunk[<match-expander>
       (λ/syntax-parse (_ tag:id (~maybe #:with-struct with-struct)
                          . structure-pat)
         (quasitemplate
          (constructor tag (?? (?@ #:with-struct with-struct))
                       #,(syntax/loc #'structure-pat
                           (structure . structure-pat)))))]

@subsection{@racket[instance creation]}

@; TODO: clean this up a bit, and explain it.
@chunk[<make-instance>
       (λ/syntax-parse
           (~and (_ (~and (~seq disambiguate …)
                          (~or (~seq #:instance)
                               (~seq #:make-instance)
                               (~seq)))
                    tag:id (~maybe #:with-struct with-struct)
                    . fields)
                 (~parse (sa:structure-args-stx-class)
                         #'(disambiguate … . fields)))
         (define-temp-ids "~a/TTemp" (sa.field …))
         (define-temp-ids "~a/arg" (sa.field …))
         (define/with-syntax c
           (if (attribute sa.type)
               (quasitemplate
                (λ ([sa.field/arg : sa.type] …)
                  : (constructor tag (?? (?@ #:with-struct with-struct))
                                 #,(syntax/loc #'fields
                                     (structure [sa.field sa.type] …)))
                  (constructor tag (?? (?@ #:with-struct with-struct))
                               #,(syntax/loc #'fields
                                   (structure #:instance
                                              [sa.field : sa.type sa.field/arg]
                                              …)))))
               (quasitemplate
                (λ #:∀ (sa.field/TTemp …) ([sa.field/arg : sa.field/TTemp] …)
                  : (constructor tag (?? (?@ #:with-struct with-struct))
                                 #,(syntax/loc #'fields
                                     (structure [sa.field sa.field/TTemp] …)))
                  (constructor tag (?? (?@ #:with-struct with-struct))
                               #,(syntax/loc #'fields
                                   (structure #:instance
                                              [sa.field sa.field/arg] …)))))))
         (if (attribute sa.value)
             #'(c sa.value …)
             #'c))]

@subsection{@racket[predicate]}

@CHUNK[<tagged-top?>
       (define-multi-id TaggedTop?
         #:else #'(λ (v) (and (ConstructorTop? v)
                              (promise? (constructor-values v))
                              (StructureTop? (force (constructor-values v))))))]

@CHUNK[<tagged?>
       (define-syntax/parse (tagged? tag (~maybe #:with-struct with-struct)
                                     field …)
         #'(λ (v) (and (constructor? tag (?? (?@ #:with-struct with-struct)) v)
                       (promise? (constructor-values v))
                       ((structure? field …)
                        (force (constructor-values v))))))]

@section{Tests}

@chunk[<test-tagged>
       (check-equal?: (match (ann (tagged t1 [x 1] [y "b"])
                                  (tagged t1 [x : Number] [y : String]))
                        [(tagged t1 [x a] [y b]) (list 'ok b a)]
                        [_ #f])
                      '(ok "b" 1))]

@chunk[<test-tagged>
       (check-equal?: (match (ann (tagged foo [x "o"] [y 3] [z 'z])
                                  (tagged foo
                                          [x String]
                                          [z 'z]
                                          [y Fixnum]))
                        [(tagged foo z x y) (list z y x)])
                      '(z 3 "o"))]

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
                    "constructor.lp2.rkt"
                    "structure.lp2.rkt")
           
           (provide tagged
                    tagged?
                    TaggedTop
                    TaggedTop?)
           
           <tagged-top>
           <tagged>
           <tagged-top?>
           <tagged?>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt")
           <test-tagged>))]