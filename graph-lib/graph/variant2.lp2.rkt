#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Algebaraic Data Types: Variant}

@(table-of-contents)

@section{Introduction}

For convenience, we write a @tc[variant] form, which is a
thin wrapper against @tc[(U (~or constructor tagged) …)].

@section{@racket[variant]}

In @tc[define-variant], we only define the type (which is
the union of all the possible constructors. We do not bind
identifiers for the constructors, for two reasons: the same
@tc[constructor]s could appear in several variants, so we
would define them twice, and it is likely that a constructor
will have the same identifier as an existing variable or
function.

@chunk[<constructor-or-tagged-stx-class>
       (begin-for-syntax
         (define-syntax-class constructor-or-tagged
           (pattern [constructor-name:id . (~or ([field:id C:colon type:expr] …)
                                                (type:expr …))])))]

@chunk[<variant>
       (define-type-expander (variant stx)
         (syntax-parse stx
           [(_ :constructor-or-tagged …)
            (template
             (U (?? (tagged constructor-name [field C type] …)
                    (constructor constructor-name type …))
                …))]))]

@section{Predicate}

@chunk[<variant?>
       (define-syntax/parse (variant? :constructor-or-tagged …)
         (template
          (λ (v) (or (?? ((tagged? constructor-name field …) v)
                         (constructor? constructor-name v))
                     …))))]

@section{@racket[define-variant]}

@chunk[<define-variant>
       (define-syntax/parse
           (define-variant variant-name
             (~optkw #:debug)
             (~maybe #:? name?)
             (~maybe #:match variant-match)
             (~and constructor-or-tagged :constructor-or-tagged) …)
         (define/with-syntax default-name? (format-id #'name "~a?" #'name))
         (define/with-syntax default-match (format-id #'name "~a-match" #'name))
         (define-temp-ids "pat" ((type …) …))
         (define-temp-ids "match-body" (constructor-name …))
         (template/debug debug
           (begin
             (define-type variant-name
               (variant [constructor-name (?? (?@ [field C type] …)
                                              (?@ type …))]
                        …))
             (define-syntax (?? variant-match default-match)
               (syntax-rules (constructor-name … (?? (?@ field …)) …)
                 [(_ v
                     [(constructor-name (?? (?@ [field pat] …)
                                            (pat …)))
                      . match-body]
                     …)
                  (match v
                    (?? [(tagged constructor-name [field pat] …) . match-body]
                        [(constructor constructor-name pat …) . match-body])
                    …)]))
             (define-multi-id (?? name? default-name?)
               #:else
               #'(variant? constructor-or-tagged …))))
         #|
         (if (andmap (λ (t) (check-remember-all 'variant t))
                     (syntax->list #'(tag …)))
             (let ()
               (define/with-syntax (stx-name …)
                 (stx-map (λ (t)
                            (cdr (assoc (syntax->datum (datum->syntax #f t))
                                        tag-name→stx-name/alist)))
                          #'(tag …)))
               (quasitemplate
                (begin
                  (define-type name (U (constructor tag type …) …))
                  (: (?? name? default-name?)
                     (→ Any Boolean :
                        #:+ (or (stx-name Any) …)
                        #:- (and (! (stx-name Any)) …)))
                  (define ((?? name? default-name?) x)
                    (or (Tagged-predicate? tag x) …)))))
             (stx-map (λ (t)
                        (remember-all-errors2 (syntax/loc t #'please-recompile)
                                              t))
                      #'(tag …)))|#)]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket/list
                                syntax/parse
                                syntax/parse/experimental/template
                                racket/syntax
                                (submod phc-toolkit untyped))
                    phc-toolkit
                    "../type-expander/multi-id.lp2.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "constructor.lp2.rkt"
                    "structure.lp2.rkt")
           
           (provide variant
                    variant?
                    define-variant)
           
           <constructor-or-tagged-stx-class>
           <variant>
           <variant?>
           <define-variant>)
         
         (require 'main)
         (provide (all-from-out 'main)))]
