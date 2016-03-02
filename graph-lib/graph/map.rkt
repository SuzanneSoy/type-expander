#lang typed/racket

(require (for-syntax racket/syntax
                     racket/function
                     syntax/parse
                     (submod "../lib/low.rkt" untyped))
         "../lib/low.rkt"
         "get.lp2.rkt"
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

(module+ test
  ;(require (submod "..")
  ;         "../lib/low.rkt")
  
  (check-equal?: (map: add1 '(1 2 3))
                 : (Listof Number)
                 '(2 3 4))
  (check-equal?: (map: (compose add1) '(1 2 3))
                 : (Listof Number)
                 '(2 3 4))
  (check-equal?: (map: (∘ identity add1) '(1 2 3))
                 : (Listof Number)
                 '(2 3 4))
  (check-equal?: (map: (∘ add1 identity) '(1 2 3))
                 : (Listof Number)
                 '(2 3 4))
  (check-equal?: (map: (∘ number->string add1) '(1 2 9))
                 : (Listof String)
                 '("2" "3" "10"))
  (check-equal?: (map: (∘ string-length number->string add1) '(1 2 9))
                 : (Listof Number)
                 '(1 1 2))
  (check-equal?: (map: car '((1 2) (2) (9 10 11)))
                 : (Listof Number)
                 '(1 2 9))
  (check-equal?: (map: (∘ add1 car) '((1 2) (2) (9 10 11)))
                 : (Listof Number)
                 '(2 3 10))
  (check-equal?: (map: (∘ string-length number->string add1 car cdr)
                       '((1 2) (2 3) (8 9 10)))
                 : (Listof Number)
                 '(1 1 2))
  (check-equal?: (map: identity '(1 2 3))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: values '(1 2 3))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: (compose) '(1 2 3))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: (compose identity) '(1 2 3))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: (∘ identity values identity values) '(1 2 3))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: (∘ length (curry map add1)) '((1 2) (3)))
                 : (Listof Number)
                 '(2 1))
  (check-equal?: (map: (curry map add1) '((1 2) (3)))
                 : (Listof (Listof Number))
                 '((2 3) (4)))
  
  (define (numlist [x : Number]) (list x))
  (check-equal?: (map: (∘ (curry map add1) numlist) '(1 2 3))
                 : (Listof (Listof Number))
                 '((2) (3) (4)))
  
  (check-equal?: (map: (∘ (curry map add1) (λ ([x : Number]) (list x)))
                       '(1 2 3))
                 : (Listof (Listof Number))
                 '((2) (3) (4)))
  
  (begin
    ;; Some of the tests below use (curry map: …), and don't work, because
    ;; typed/racket wraps the map: identifier with a contract, so the identifier
    ;; seen outside the module is not the same as the one used in the
    ;; syntax-parse ~literal clause.
    
    (check-equal?: (map: (curry map add1) '((1 2 3) (4 5)))
                   : (Listof (Listof Number))
                   '((2 3 4) (5 6)))
    #;(check-equal?: (map: (curry map: add1) '((1 2 3) (4 5)))
                     : (Listof (Listof Number))
                     '((2 3 4) (5 6)))
    
    (check-equal?: (map: (curry map (compose number->string add1))
                         '((1 2 3) (4 5)))
                   : (Listof (Listof String))
                   '(("2" "3" "4") ("5" "6")))
    #;(check-equal?: (map: (curry map: (compose number->string add1))
                           '((1 2 3) (4 5)))
                     : (Listof (Listof String))
                     '(("2" "3" "4") ("5" "6")))
    
    (check-equal?: (map: add1 '(1 2 3))
                   : (Listof Number)
                   '(2 3 4))
    
    (check-equal?: (map: car '((1 a) (2 b) (3 c)))
                   : (Listof Number)
                   '(1 2 3))
    
    (check-equal?: (map: (curry map car) '([{1 a} {2 b}] [{3 c}]))
                   : (Listof (Listof Number))
                   '([1 2] [3]))
    #;(check-equal?: (map: (curry map: car) '([{1 a} {2 b}] [{3 c}]))
                     : (Listof (Listof Number))
                     '([1 2] [3]))
    
    (check-equal?: (map: (curry map (curry map car))
                         '([((1 a) (2 b)) ((3 c))] [((4))]))
                   : (Listof (Listof (Listof Number)))
                   '([(1 2) (3)] [(4)]))
    #;(check-equal?: (map: (curry map (curry map: car))
                           '([((1 a) (2 b)) ((3 c))] [((4))]))
                     : (Listof (Listof (Listof Number)))
                     '([(1 2) (3)] [(4)]))
    #;(check-equal?: (map: (curry map: (curry map car))
                           '([((1 a) (2 b)) ((3 c))] [((4))]))
                     : (Listof (Listof (Listof Number)))
                     '([(1 2) (3)] [(4)]))
    #;(check-equal?: (map: (curry map: (curry map: car))
                           '([((1 a) (2 b)) ((3 c))] [((4))]))
                     : (Listof (Listof (Listof Number)))
                     '([(1 2) (3)] [(4)])))
  
  (check-equal?: (map: car '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: cdr '((1 b x) (2 c) (3 d)))
                 : (Listof (Listof Symbol))
                 '((b x) (c) (d)))
  (check-equal?: (map: car (map: cdr '((1 b x) (2 c) (3 d))))
                 : (Listof Symbol)
                 '(b c d))
  (check-equal?: (map: (compose) '((1 b x) (2 c) (3 d)))
                 : (Listof (Listof (U Number Symbol)))
                 '((1 b x) (2 c) (3 d)))
  (check-equal?: (map: (compose car) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: (compose cdr) '((1 b x) (2 c) (3 d)))
                 : (Listof (Listof Symbol))
                 '((b x) (c) (d)))
  (check-equal?: (map: (compose car cdr) '((1 b x) (2 c) (3 d)))
                 : (Listof Symbol)
                 '(b c d))
  (check-equal?: (map: (compose add1 car) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(2 3 4))
  #|
  (check-equal?: (map: + '(1 2 3) '(4 5 6))
                 : (Listof Number)
                 '(5 7 9))|#)