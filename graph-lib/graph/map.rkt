#lang typed/racket

(require (for-syntax racket/syntax
                     racket/function
                     syntax/parse
                     (submod "../lib/low.rkt" untyped))
         "../lib/low.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt")

(provide car!
         cdr!
         map:
         compose-maps)

(module+ private-tests
  (provide λdeep-map
           deep-map
           deep-map-auto))

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
            (∀ (∀-type …)
               (→ (→ A B) A B)
               ;; Use the type below to allow identity functions, but it's more
               ;; heavy on the typechecker
               #;(case→ (→ (→ A B) A B)
                        (→ (→ A A) A A))))]
    [(_ (~and norun #:norun) … {∀-type:id …} A:expr B:expr d:≥0)
     (define/with-syntax local-map (generate-temporary #'map))
     #`(dbg norun …
            (let ()
              (: local-map
                 (∀ (∀-type …)
                    (→ (→ A B) (Deep-Listof d A) (Deep-Listof d B))
                    ;; Use the type below to allow identity functions, but it's
                    ;; more heavy on the typechecker
                    #;(case→ (→ (→ A B) (Deep-Listof d A) (Deep-Listof d B))
                             (→ (→ A A) (Deep-Listof d A) (Deep-Listof d A)))))
              (define (local-map f l)
                (if (null? l)
                    '()
                    (cons ((λdeep-map {∀-type …} A B #,(sub1 (syntax-e #'d)))
                           f (car l))
                          (local-map f (cdr l)))))
              local-map))]))

(define-syntax (deep-map stx)
  (syntax-parse stx
    [(_ {∀-type:id …} A:expr B:expr d:≥0 f:expr l:expr)
     (syntax/loc #'f ((λdeep-map {∀-type …} A B d) f l))]))

;; We provide hints for the types of some common functions

(define-type-expander (ArgOf stx)
  (syntax-parse stx
    [(_ (~literal length) T:expr R) #'(Listof Any)]
    [(_ (~literal car) T:expr R) #'(Pairof T Any)]
    [(_ (~literal car!) T:expr R) #'(U (Listof T) (Pairof T Any))]
    [(_ (~literal cdr) T:expr R) #'(Pairof Any T)]
    [(_ (~literal list) T:expr R) #'T]
    [(_ ((~literal λget) f …) T:expr R) #'(has-get T f …)]
    ;; Default case:
    [(_ f:expr T:expr R) #'T]))

(define-type-expander (ResultOf stx)
  (syntax-parse stx
    [(_ (~literal length) T:expr R) #'Index]
    [(_ (~literal car) T:expr R) #'T]
    [(_ (~literal car!) T:expr R) #'T]
    [(_ (~literal cdr) T:expr R) #'T]
    [(_ (~literal list) T:expr R) #'(List T)]
    [(_ ((~literal λget) f …) T:expr R) #'(result-get T f …)]
    ;; Default case:
    [(_ f:expr T:expr R) #'R]))

(define-syntax (substitute-function stx)
  (syntax-parse stx
    [(_ (~literal list)) #'(λ #:∀ (X) ([x : X]) : (List X) (list x))]
    ;; Default case:
    [(_ f:expr) #'f]))

(define-syntax/parse (deep-map-auto d:≥0 f l)
  #'(deep-map {A B} (ArgOf f A B) (ResultOf f A B) d (substitute-function f) l))

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

(define-for-syntax (transform-map: depth stx)
  (syntax-parse stx
    [((~literal curry) (~literal map) f:expr)
     (transform-map: (add1 depth) #'f)]
    [((~literal compose) f:expr …)
     (define/syntax-parse (([dd ff] …) …)
       (stx-map (curry transform-map: depth) #'(f …)))
     #`[(dd ff) … …]]
    [(~literal identity) #'[]]
    [(~literal values) #'[]]
    [f:expr
     #`[(#,depth f)]]))

(define-syntax (map: stx)
  (syntax-parse stx
    [(_ f l) #`(compose-maps #,(transform-map: 1 #'f) [l])]))
