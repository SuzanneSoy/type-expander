#lang typed/racket

(require (for-syntax racket/syntax
                     racket/function
                     syntax/stx
                     syntax/parse
                     syntax/parse/experimental/template
                     "../lib/low-untyped.rkt")
         "../lib/low.rkt"
         "graph4.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt")

(module m typed/racket
  (provide car! cdr!)
  
  (: car! (∀ (A) (→ (U (Listof A) (Pairof A Any)) A)))
  (define (car! x) (if (pair? x)
                       (car x)
                       (car x)))
  
  (: cdr! (∀ (A) (case→ (→ (Listof A) (Listof A))
                        (→ (Pairof Any A) A))))
  (define (cdr! x) (cdr x)))

(require 'm)
(provide (all-from-out 'm))

(provide map: compose-maps)

(define-syntax (dbg stx)
  (syntax-parse stx
    [(_ (~optional (~and norun #:norun)) code)
     (if (attribute norun)
         #'(ann 'code Any)
         #'code)]))

(begin-for-syntax
  (define-syntax-class >0 (pattern :exact-positive-integer)))

(begin-for-syntax
  (define-syntax-class ≥0 (pattern :exact-nonnegative-integer)))

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
    [(_ (~and norun #:norun) … {∀-type:id …} A:expr B:expr d:≥0)
     (define/with-syntax local-map (generate-temporary #'map))
     #`(dbg norun …
            (let ()
              (: local-map (∀ (∀-type …) (→ (→ A B)
                                            (Deep-Listof d A)
                                            (Deep-Listof d B))))
              (define (local-map f l)
                (if (null? l)
                    '()
                    (cons ((λdeep-map {∀-type …} A B #,(sub1 (syntax-e #'d)))
                           f (car l))
                          (local-map f (cdr l)))))
              local-map))]))

(module+ test
  (check-equal?: ((λdeep-map {A B} A B 3) add1 '([{1} {2 3}] [{4}]))
                 : (Listof (Listof (Listof Number)))
                 '([{2} {3 4}] [{5}])))

(define-syntax (deep-map stx)
  (syntax-parse stx
    [(_ {∀-type:id …} A:expr B:expr d:≥0 f:expr l:expr)
     (syntax/loc #'f ((λdeep-map {∀-type …} A B d) f l))]))

(module+ test
  (check-equal?: (deep-map {A B} A B 3 add1 '([{1} {2 3}] [{4}]))
                 : (Listof (Listof (Listof Number)))
                 '([{2} {3 4}] [{5}])))

(module+ test
  (check-equal?: (deep-map {A B} A B 0 add1 '7)
                 : Number
                 8))

;; We provide hints for the types of some common functions

(define-type-expander (ArgOf stx)
  (syntax-parse stx
    [(_ (~literal length) T:expr R) #'(Listof Any)]
    [(_ (~literal car) T:expr R) #'(Pairof T Any)]
    [(_ (~literal car!) T:expr R) #'(U (Listof T) (Pairof T Any))]
    [(_ (~literal cdr) T:expr R) #'(Pairof Any T)]
    [(_ (~literal list) T:expr R) #'T]
    ;; Default case:
    [(_ f:expr T:expr U) #'T]))

(define-type-expander (ResultOf stx)
  (syntax-parse stx
    [(_ (~literal length) T:expr R) #'Index]
    [(_ (~literal car) T:expr R) #'T]
    [(_ (~literal car!) T:expr R) #'T]
    [(_ (~literal cdr) T:expr R) #'T]
    [(_ (~literal list) T:expr R) #'(List T)]
    ;; Default case:
    [(_ f:expr T:expr R) #'R]))

(define-syntax (substitute-function stx)
  (syntax-parse stx
    [(_ (~literal list)) #'(λ #:∀ (X) ([x : X]) : (List X) (list x))]
    ;; Default case:
    [(_ f:expr) #'f]))

(define-syntax/parse (deep-map-auto d:≥0 f l)
  #'(deep-map {A B} (ArgOf f A B) (ResultOf f A B) d (substitute-function f) l))

(module+ test
  (check-equal?: (deep-map-auto 2 length '([{1} {2 3}] [{4}]))
                 : (Listof (Listof Index))
                 '([1 2] [1])))

(module+ test
  (check-equal?: (deep-map-auto 2 car '([{1} {2 3}] [{4}]))
                 : (Listof (Listof Number))
                 '([1 2] [4])))

(module+ test
  (check-equal?: (deep-map-auto 2 list '([1 2] [3]))
                 : (Listof (Listof (Listof Number)))
                 '([{1} {2}] [{3}])))

#;(module+ test
    (check-equal?: (deep-map-auto 3 add1 (deep-map-auto 2 list '([1 2] [3])))
                   : (Listof (Listof (Listof Number)))
                   '([{1} {2}] [{3}])))

(module+ test
  (check-equal?: (deep-map-auto 1 length
                                (deep-map-auto 2 car
                                               (deep-map-auto 2 list
                                                              '([1 2] [3]))))
                 : (Listof Index)
                 '(2 1)))

;; Now we turn all map: calls into the form
;; (compose-maps [(d f) …] [l …])

(define-syntax (compose-maps stx)
  (syntax-parse stx
    [(_ [] [l])
     #'l]
    [(_ [] [l:expr …])
     #'(values l …)]
    [(_ [(d:≥0 f:expr) (d-rest:≥0 f-rest:expr) …] [l:expr …])
     #'(deep-map-auto d f (compose-maps [(d-rest f-rest) …] [l …]))]))

(module+ test
  (check-equal?: (compose-maps [(2 car!) (3 add1) (3 add1) (2 list)]
                               ['([1 2] [3])])
                 : (Listof (Listof Number))
                 '([3 4] [5])))

(define-for-syntax (transform-map: depth stx)
  (syntax-parse stx
    [((~literal curry) (~literal map) f:expr)
     (transform-map: (add1 depth) #'f)]
    [((~literal ∘) f:expr …)
     (define/syntax-parse (([dd ff] …) …)
       (stx-map (curry transform-map: depth) #'(f …)))
     #`[(dd ff) … …]]
    [f:expr
     #`[(#,depth f)]]))

(define-syntax (map: stx)
  (syntax-parse stx
    [(_ f l) #`(compose-maps #,(transform-map: 1 #'f) [l])]))

(module+ test
  (check-equal?: (map: car '((1 a) (2 b) (3 c)))
                 : (Listof Number)
                 '(1 2 3)))

(module+ test
  (check-equal?: (map: (∘ (∘ add1)
                          length
                          (curry map car)
                          (curry map list)
                          (curry map (∘)))
                       '([1 2] [3]))
                 : (Listof Number)
                 '(3 2)))