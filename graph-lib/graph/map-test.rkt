#lang typed/racket

(module test typed/racket
  (require "map.rkt"
           (submod "map.rkt" private-tests))
  (require (submod "graph-test.rkt" test)
           "get.lp2.rkt"
           "map.rkt"
           phc-toolkit
           "../type-expander/type-expander.lp2.rkt")
  
  (begin
    (check-equal?: ((λdeep-map {A B} A B 3) add1 '([{1} {2 3}] [{4}]))
                   : (Listof (Listof (Listof Number)))
                   '([{2} {3 4}] [{5}])))
  
  ;; deep-map
  (begin
    (check-equal?: (deep-map {A B} A B 3 add1 '([{1} {2 3}] [{4}]))
                   : (Listof (Listof (Listof Number)))
                   '([{2} {3 4}] [{5}]))
    
    
    (check-equal?: (deep-map {A B} A B 0 add1 '7)
                   : Number
                   8))
  
  ;; deep-map-auto
  (begin
    (check-equal?: (deep-map-auto 2 length '([{1} {2 3}] [{4}]))
                   : (Listof (Listof Index))
                   '([1 2] [1]))
    
    (check-equal?: (deep-map-auto 2 car '([{1} {2 3}] [{4}]))
                   : (Listof (Listof Number))
                   '([1 2] [4]))
    
    (check-equal?: (deep-map-auto 2 list '([1 2] [3]))
                   : (Listof (Listof (Listof Number)))
                   '([{1} {2}] [{3}]))
    
    #;(check-equal?: (deep-map-auto 3 add1 (deep-map-auto 2 list '([1 2] [3])))
                     : (Listof (Listof (Listof Number)))
                     '([{1} {2}] [{3}]))
    
    (check-equal?: (deep-map-auto 1 length
                                  (deep-map-auto 2 car
                                                 (deep-map-auto 2 list
                                                                '([1 2] [3]))))
                   : (Listof Index)
                   '(2 1)))
  
  ;; compose-maps
  (begin
    (check-equal?: (compose-maps [(2 car!) (3 add1) (3 add1) (2 list)]
                                 ['([1 2] [3])])
                   : (Listof (Listof Number))
                   '([3 4] [5])))
  
  ;; map:
  (begin
    (check-equal?: (map: car '((1 a) (2 b) (3 c)))
                   : (Listof Number)
                   '(1 2 3))
    
    (check-equal?: (map: (∘ (∘ add1)
                            length
                            (curry map car)
                            (curry map list)
                            (curry map (∘)))
                         '([1 2] [3]))
                   : (Listof Number)
                   '(3 2)))
  
  ;; map:
  (begin
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
      ;; Some of the tests below use (curry map: …) instead of (curry map …).
      ;; The colon `map:` version does not work, because typed/racket wraps the
      ;; `map:` identifier with a contract, so the identifier seen outside the
      ;; module is not the same as the one used in the syntax-parse ~literal
      ;; clause.
      
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
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (check-equal?:
   (map: (curry map (gr #:? House))
         (map: (λget houses) (get g streets)))
   '((#t #t) (#t)))
  
  (check-equal?:
   (map: (λget houses … owner name) (get g streets))
   '(("Amy" "Anabella") ("Jack")))
  
  (check-equal?:
   (map: (∘ (curry map (∘ (λget name) (λget owner)))
            (λget houses))
         (get g streets))
   '(("Amy" "Anabella") ("Jack")))
  
  (check-equal?:
   (map: (∘ (curry map (∘ string-length (λget name) (λget owner)))
            (λget houses))
         (get g streets))
   '((3 8) (4)))
  
  (check-equal?:
   (map: (curry map (gr #:? House))
         (map: (compose (curry map identity)
                        (λget houses …))
               (get g streets)))
   '((#t #t) (#t)))
  
  ;; Can be allowed by changing (→ (→ A B) A B) in a couple of places in map: to
  ;; a case→, but it's more heavy on the typechecker, and it's an uncommon case.
  ;(map: (compose (λget houses …) (λ #:∀ (A) ([x : A]) x)) (get g streets))
  (check-equal?:
   (map: (curry map (gr #:? Person))
         (map: (compose (curry map (λget owner))
                        (λget houses …))
               (get g streets)))
   '((#t #t) (#t)))
  
  (check-equal?:
   (map: (λget houses … owner name) (get g streets))
   '(("Amy" "Anabella") ("Jack"))))