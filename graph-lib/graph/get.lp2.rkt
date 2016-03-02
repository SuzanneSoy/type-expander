#lang debug scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Syntactic sugar for the @racket[graph] macro}

@(table-of-contents)

@section{Introduction}

We allow not just identifiers having the @tc[c…r] syntax, but also those
matching one of the predefined identifiers, so that @tc[(rename-in [car hd])]
is correctly taken into account. Note that @tc[car] would still work, because we
also match identifiers with the @tc[c…r] syntax, as a fallback or for chains of
more than 4 letters.

@CHUNK[<c…r-syntax-class>
       (define-syntax-class c…r
         #:attributes ([expanded 1] [reverse-expanded 1])
         (pattern :c…r1
                  #:with (reverse-expanded …)
                  (reverse (syntax->list #'(expanded …)))))
       
       (define-syntax-class c…r1
         ;(pattern (~literal car) #:with (expanded …) #'(car))
         ;(pattern (~literal cdr) #:with (expanded …) #'(cdr))
         (pattern (~literal caar) #:with (expanded …) #'(car car))
         (pattern (~literal cdar) #:with (expanded …) #'(cdr car))
         (pattern (~literal cadr) #:with (expanded …) #'(car cdr))
         (pattern (~literal cddr) #:with (expanded …) #'(cdr cdr))
         (pattern (~literal caaar) #:with (expanded …) #'(car car car))
         (pattern (~literal cdaar) #:with (expanded …) #'(cdr car car))
         (pattern (~literal cadar) #:with (expanded …) #'(car cdr car))
         (pattern (~literal cddar) #:with (expanded …) #'(cdr cdr car))
         (pattern (~literal caadr) #:with (expanded …) #'(car car cdr))
         (pattern (~literal cdadr) #:with (expanded …) #'(cdr car cdr))
         (pattern (~literal caddr) #:with (expanded …) #'(car cdr cdr))
         (pattern (~literal cdddr) #:with (expanded …) #'(cdr cdr cdr))
         (pattern (~literal caaaar) #:with (expanded …) #'(car car car car))
         (pattern (~literal cdaaar) #:with (expanded …) #'(cdr car car car))
         (pattern (~literal cadaar) #:with (expanded …) #'(car cdr car car))
         (pattern (~literal cddaar) #:with (expanded …) #'(cdr cdr car car))
         (pattern (~literal caadar) #:with (expanded …) #'(car car cdr car))
         (pattern (~literal cdadar) #:with (expanded …) #'(cdr car cdr car))
         (pattern (~literal caddar) #:with (expanded …) #'(car cdr cdr car))
         (pattern (~literal cdddar) #:with (expanded …) #'(cdr cdr cdr car))
         (pattern (~literal caaadr) #:with (expanded …) #'(car car car cdr))
         (pattern (~literal cdaadr) #:with (expanded …) #'(cdr car car cdr))
         (pattern (~literal cadadr) #:with (expanded …) #'(car cdr car cdr))
         (pattern (~literal cddadr) #:with (expanded …) #'(cdr cdr car cdr))
         (pattern (~literal caaddr) #:with (expanded …) #'(car car cdr cdr))
         (pattern (~literal cdaddr) #:with (expanded …) #'(cdr car cdr cdr))
         (pattern (~literal cadddr) #:with (expanded …) #'(car cdr cdr cdr))
         (pattern (~literal cddddr) #:with (expanded …) #'(cdr cdr cdr cdr))
         (pattern id:id
                  #:when (regexp-match #rx"^c[ad][ad]*r$"
                                       (identifier→string #'id))
                  #:with (expanded …)
                  (map (λ (c)
                         (cond [(equal? c #\a) #'car]
                               [(equal? c #\d) #'cdr]
                               [<c*r-error>]))
                       (string->list
                        (cadr
                         (regexp-match #rx"^c([ad][ad]*)r$"
                                       (identifier→string #'id)))))))]

Although this should not happen, by construction, we check for both cases
(@tc[#\a] and @tc[#\d]) for each character of a @tc[c…r] identifier, and
otherwise throw an error:

@chunk[<c*r-error>
       (raise-syntax-error 'c*r "expected a or d" #'id)]

@chunk[<get>
       (define-multi-id has-get
         #:type-expander <type-for-get>)
       (define-multi-id result-get
         #:type-expander <result-type-for-get>)
       (define-multi-id get
         ;#:type-expander <type-for-get>
         #:call
         (λ (stx)
           (syntax-parse stx
             [(_ v:expr)
              #'v]
             [(_ v:expr (~and c?r (~or (~lit car) (~lit cdr))) other-fields …)
              #'(get (c?r v) other-fields …)]
             [(_ v:expr c…r:c…r other-fields …)
              #`(get v c…r.reverse-expanded … other-fields …)]
             [(_ v:expr (~literal …) other-fields …)
              #'((curry-map T
                            (result-get T other-fields …)
                            (has-get T other-fields …)
                            (λget other-fields …))
                 v)
              #;#'((λ #:∀ (T) ([l : (Listof (has-get T other-fields …))])
                   : (Listof (result-get T other-fields …))
                   ((inst map
                          (result-get T other-fields …)
                          (has-get T other-fields …))
                    (λget other-fields …)
                    l))
                 v)]
             [(_ v:expr field other-fields:id …)
              #'(let ([v-cache v])
                  (cond <get-tagged>
                        <get-promise>
                        <get-plain-struct>))])))]

@chunk[<get-tagged>
       [((make-predicate (List Symbol Any)) v-cache)
        (get (structure-get (cadr v-cache) field) other-fields …)]]

@chunk[<get-promise>
       [(promise? v-cache)
        (let ([f-cache (force v-cache)])
          (if ((make-predicate (List Symbol Any)) f-cache)
              (get (structure-get (cadr f-cache) field) other-fields …)
              (get (structure-get f-cache field) other-fields …)))]]

@chunk[<get-plain-struct>
       [else
        (get (structure-get v-cache field) other-fields …)]]

@chunk[<test-get>
       (check-equal? 'TODO 'TODO)]

@section{@racket[λget]}

@chunk[<λget>
       (define-syntax (λget stx)
         (syntax-parse stx
           [(_ field-or-accessor …)
            #'(ann (λ (v) (get v field-or-accessor …))
                   (∀ (T) (→ (has-get T field-or-accessor …)
                             (result-get T field-or-accessor …))))]))]

The type for the function generated by @tc[λget] mirrors the cases from
@tc[get].

@; TODO: To avoid the n⁴ code size complexity (with n being the number of fields
@; in the expression (λget f₁ … fₙ), maybe we should always wrap structures in a
@; list with a dummy symbol as the first element, and wrap that in a promise
@; that just returns the value. That way, we'll always fall in the
@; @tc[(Promise (List <r>))] case. Using a named type of the form
@; @tc[(define-type (maybe-wrapped S) (U S (List Symbol S)
@;                                         (Promise (U S (List Symbol S)))))]
@; won't work, because TR inlines these, unless they are recursive.
@; We could otherwise try to make sure that the user never sees a Promise, and
@; always force it when we return one (and node types would be
@; @tc[(U with-fields=promises with-fields=with-promises)
@; Or we could put in a fake piece of recursion to prevent TR from expanding the
@; type, but that behaviour could change in the future:
@; @tc[(define-type (maybe-wrapped S) (U S
@;                                       (List Symbol S)
@;                                       (Promise (U S (List Symbol S)))
@;                                       (→ (maybe-wrapped S) unforgeable))))]

@chunk[<type-for-get>
       (λ (stx)
         (syntax-parse stx
           [(_ T:expr) #'T]
           <type-for-get-pairs>
           <type-for-get-field>))]

@chunk[<type-for-get-pairs>
       ;; TODO: car, cdr, c…r.
       [(_ T:expr (~literal car) other-fields:id …)
        #'(Pairof (has-get T other-fields …) Any)]
       [(_ T:expr (~literal cdr) other-fields:id …)
        #'(Pairof Any (has-get T other-fields …))]
       [(_ T:expr c…r:c…r other-fields:id …)
        #'(has-get T c…r.reverse-expanded … other-fields …)]
       [(_ T:expr (~literal …) other-fields:id …)
        #'(Listof (has-get T other-fields …))]]

@chunk[<type-for-get-field>
       [(_ T:expr field:id other-fields:id …)
        #'(Promise
           (List Symbol
                 (structure-supertype [field : (has-get T other-fields …)])))]]

@chunk[<result-type-for-get>
       (λ (stx)
         (syntax-parse stx
           [(_ T:expr) #'T]
           [(_ T:expr (~literal …) other-fields:id …)
            #'(Listof (result-get T other-fields …))]
           [(_ T:expr
               (~or (~lit car)
                    (~lit cdr)
                    :c…r
                    field:id)
               other-fields:id …)
            #'(result-get T other-fields …)]))]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              racket/syntax
                              (submod "../lib/low.rkt" untyped))
                  "../lib/low.rkt"
                  "structure.lp2.rkt"
                  "variant.lp2.rkt"
                  "graph.lp2.rkt"
                  "../type-expander/multi-id.lp2.rkt"
                  "../type-expander/type-expander.lp2.rkt"
                  "map1.rkt")
         (provide get
                  λget
                  has-get
                  result-get)
         
         (begin-for-syntax
           <c…r-syntax-class>)
         
         <get>
         <λget>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  typed/rackunit)
         
         <test-get>)]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]