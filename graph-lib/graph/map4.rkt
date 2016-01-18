#lang typed/racket

(require (for-syntax racket/syntax
                     syntax/stx
                     syntax/parse
                     syntax/parse/experimental/template
                     "../lib/low-untyped.rkt")
         "../lib/low.rkt"
         "graph4.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt")

(begin-for-syntax
  (define-syntax-class >0
    (pattern v #:when (exact-positive-integer? (syntax-e #'v)))))

(begin-for-syntax
  (define-syntax-class ≥0
    (pattern v #:when (exact-integer? (syntax-e #'v)))))

(define-type-expander (Deep-Listof stx)
  (syntax-parse stx
    [(_ 0 T)
     #'T]
    [(_ d:>0 T)
     #`(Listof (Deep-Listof #,(sub1 (syntax-e #'d)) T))]))

(define-syntax (λdeep-map stx)
  (syntax-parse stx
    [(_ {∀-type:id …} A:expr B:expr 0)
     #'(ann (λ (f x) (f x))
            (∀ (∀-type …) (→ (→ A B) A B)))]
    [(_ {∀-type:id …} A:expr B:expr d:exact-integer)
     (define/with-syntax local-map (generate-temporary #'map))
     #`(let ()
         (: local-map (∀ (∀-type …) (→ (→ A B)
                                       (Deep-Listof d A)
                                       (Deep-Listof d B))))
         (define (local-map f l)
           (if (null? l)
               '()
               (cons ((λdeep-map {∀-type …} A B #,(sub1 (syntax-e #'d)))
                      f (car l))
                     (local-map f (cdr l)))))
         local-map)]))

(module+ test
  (check-equal?: ((λdeep-map {A B} A B 3) add1 '([{1} {2 3}] [{4}]))
                 : (Listof (Listof (Listof Number)))
                 '([{2} {3 4}] [{5}])))

#|
(define-syntax (deep-map stx)
  (syntax-parse stx
    [(_ d:≥0 f:expr l:expr)
     (syntax/loc #'f ((λdeep-map d) f l))]))

(deep-map 3 add1 '([{1} {2 3}] [{4}]))

;; Now we turn all map: calls into the form
;; (compose-maps [(d f) …] [l …])

(define-syntax (compose-maps stx)
  (syntax-parse stx
    [(_ [] [l])
     #'l]
    [(_ [] [l:expr …])
     #'(values l …)]
    [(_ [(d:≥0 f:expr) (d-rest:≥0 f-rest:expr) …] [l:expr …])
     #'(deep-map d f (compose-maps [(d-rest f-rest) …] [l …]))]))

(compose-maps [(3 add1) (3 add1)] ['([{1} {2 3}] [{4}])])
|#