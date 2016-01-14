#lang debug typed/racket

(require (for-syntax racket/syntax
                     syntax/stx
                     syntax/parse
                     syntax/parse/experimental/template
                     "../lib/low-untyped.rkt")
         "../lib/low.rkt"
         "map1.rkt"
         "graph4.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt")

(provide map:)

(begin-for-syntax
  (define-syntax-class lam
    (pattern (~or (~literal λ) (~literal lambda))))
  (define-syntax-class mapp
    (pattern (~or (~literal map) (~literal map:)))))

(define-for-syntax (remove-identities stx)
  (syntax-parse stx
    [() #'()]
    [((~or (~lit identity) (~lit values) (~lit compose)) . rest)
     (remove-identities #'rest)]
    [([(~literal compose) . fs] . rest)
     (define/with-syntax cleaned-fs (remove-identities #'fs))
     (syntax-parse #'cleaned-fs
       [() (remove-identities #'rest)]
       [(one-f) #`(one-f . #,(remove-identities #'rest))]
       [some-fs #`((compose . some-fs) . #,(remove-identities #'rest))])]
    [(f . rest)
     #`(f . #,(remove-identities #'rest))]))

;; TODO: check that we don't bork the literals identity, values and compose
;; inside macros or function calls, or alter them in any other way, e.g.
;;     (map: (compose identity (λ (values) (+ values 1)) identity) '(1 2 3))
;; or
;;     (define (calltwice f) (λ (x) (f (f x))))
;;     (map: (compose (calltwice identity)) '(1 2 3))
;; Although a poor variable name choice, the two occurences of "values" in the
;; first example shouldn't be altered, and the λ itself shouldn't be touched.
;; In the second one, everything inside the calltwice function call should be
;; left intact.
(define-for-syntax (remove-identities1 stx)
  (syntax-parse (remove-identities #`(#,stx))
    [() #'identity]
    [(f) #'f]))

(begin-for-syntax
  (define-syntax-class map-info
    (pattern (_ #:in in-type
                #:out out-type
                #:∀ (∀-type …)
                #:arg-funs ([arg-fun
                             param-fun
                             (~optional (~and auto-in #:auto-in))
                             fun-in fun-out] …)
                #:funs [fun …]))))

(define-for-syntax (:map* stx* stx-&l… stx-out)
  (if (stx-null? stx*)
      '()
      (syntax-parse (:map (stx-car stx*) stx-&l… stx-out)
        [info:map-info
         (let ([r (:map* (stx-cdr stx*) stx-&l… #'info.in-type)]
               [auto (attribute info.auto-in)])
           (if (and (not (null? auto)) (car auto) (not (null? r)))
               (syntax-parse (car r)
                 [r-info:map-info
                  (let ([intact #'([info.arg-fun
                                    info.param-fun
                                    info.fun-in ;;;
                                    info.fun-out] …)]
                        [replaced #'([info.arg-fun
                                      info.param-fun
                                      r-info.out-type ;;info.fun-in ;;;
                                      info.fun-out] …)])
                    (cons #`(info #:in info.in-type
                                  #:out info.out-type
                                  #:∀ (info.∀-type …)
                                  #:arg-funs (#,(stx-car replaced)
                                              #,@(stx-cdr intact))
                                  #:funs [info.fun …])
                          r))])
               (cons #'info r)))])))

(define-for-syntax (:map stx stx-&l… stx-out)
  (define/with-syntax (&l …) stx-&l…)
  (define/with-syntax out stx-out)
  (syntax-parse (remove-identities1 stx)
    [(~literal car)
     #'(info #:in (Pairof out Any) #:out out #:∀ ()
             #:arg-funs () #:funs (car))]
    [(~literal cdr)
     #'(info #:in (Pairof Any out) #:out out #:∀ ()
             #:arg-funs () #:funs (cdr))]
    ;; TODO: should remove `identity` completely, doing (map identity l) is
    ;; useless appart for constraining the type, but it's an ugly way to do so.
    [(~literal identity)
     #'(info #:in out #:out out #:∀ ()
             #:arg-funs () #:funs (identity))]
    [((~literal compose) f …)
     (syntax-parse (:map* #'(f …) #'(&l …) #'out)
       [(~and (_ … rightmost:map-info) (leftmost:map-info . _) (:map-info …))
        #'(info #:in rightmost.in-type
                #:out leftmost.out-type
                #:∀ (∀-type … …)
                #:arg-funs ([arg-fun param-fun fun-in fun-out] … …)
                #:funs (fun … …))])]
    [((~literal curry) :mapp f)
     (syntax-parse (internal-map: #'f #'(&l …) #'out)
       [(i:map-info . code)
        #'(info #:in (Listof i.in-type)
                #:out (Listof out)
                #:∀ [i.∀-type …]; i.out-type
                #:arg-funs [(i.arg-fun i.param-fun i.fun-in i.fun-out) …]
                #:funs [(code i.fun … _)])])]
    [(~literal length)
     (define-temp-ids "&~a" f)
     (define-temp-ids "~a/in" f)
     #'(info #:in f/in #:out out #:∀ (f/in)
             #:arg-funs ([(λ ([l : (Listof Any)]) (length l))
                          &f
                          #:auto-in f/in
                          out])
             #:funs (&f))]
    [f
     (define-temp-ids "&~a" f)
     (define-temp-ids "~a/in" f)
     #'(info #:in f/in #:out out #:∀ (f/in)
             #:arg-funs ([f &f #:auto-in f/in out]) #:funs (&f))]))

(define-syntax (apply-compose stx)
  (syntax-parse stx
    [(_ [] [a …])
     #'(values a …)]
    [(_ [f … (f-last x … (~literal _) y …)] [a …])
     #'(apply-compose [f …] [(f-last x … a … y …)])]
    [(_ [f … f-last] [a …])
     #'(apply-compose [f …] [(f-last a …)])]))

(define-for-syntax (internal-map: stx-f stx-&l… stx-out)
  (define/with-syntax f stx-f)
  (define/with-syntax (&l …) stx-&l…)
  (define/with-syntax out stx-out)
  (syntax-parse (:map #'f #'(&l …) #'out)
    [(~and i :map-info)
     (cons #'i
           #'(let ()
               (: map1 (∀ [out ∀-type …]
                          (→ (→ fun-in fun-out) …
                             (Listof in-type)
                             (Listof out-type))))
               (define (map1 param-fun … &l …)
                 (if (or (null? &l) …)
                     '()
                     (cons (apply-compose [fun …] [(car &l) …])
                           (map1 param-fun … (cdr &l) …))))
               map1))]));(map1 arg-fun … . ls)

;; TODO: inefficient at compile-time: we run (:map #'f #'Out) twice.
;; Plus it could cause some bugs because of differing #'Out.
(define-syntax (map: stx)
  (syntax-parse stx
    [(_ (~optional (~and norun (~literal norun))) f l …)
     (define-temp-ids "&~a" (l …))
     (syntax-parse (internal-map: #'f #'(&l …) #'Out)
       [(:map-info . code)
        (if (attribute norun)
            #'(ann '(code arg-fun … l …) Any)
            #'(code arg-fun … l …))])]))

(map: add1 '(1 2 3))
(map: (compose add1) '(1 2 3))
(map: (∘ identity add1) '(1 2 3))
(map: (∘ add1 identity) '(1 2 3))
(map: (∘ number->string add1) '(1 2 9))
(map: (∘ string-length number->string add1) '(1 2 9))
(map: car '((1 2) (2) (9 10 11)))
(map: (∘ add1 car) '((1 2) (2) (9 10 11)))
(map: (∘ string-length number->string add1 car cdr)
      '((1 2) (2 3) (9 10 11)))
(map: identity '(1 2 3))
(map: values '(1 2 3))
(map: (compose) '(1 2 3))
(map: (compose identity) '(1 2 3))
(map: (∘ identity values identity values) '(1 2 3))
(map: (∘ length (curry map add1)) '((1 2) (3)))

(map: (curry map add1) '((1 2) (3)))

(define (numlist [x : Number]) (list x))
(map: (∘ (curry map add1) numlist) '(1 2 3))
(map: (∘ (curry map add1) (λ ([x : Number]) (list x))) '(1 2 3))


(module* test typed/racket
  (require (submod "..")
           "../lib/low.rkt")
  
  (check-equal?: (map: car '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: cdr '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '((b x) (c) (d)))
  (check-equal?: (map: car (map: cdr '((1 b x) (2 c) (3 d))))
                 : (Listof Number)
                 '(b c d))
  (check-equal?: (map: (compose) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '((1 b x) (2 c) (3 d)))
  (check-equal?: (map: (compose car) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: (compose cdr) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '((b x) (c) (d)))
  (check-equal?: (map: (compose car cdr) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(b c d))
  (check-equal?: (map: (compose add1 car) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(2 3 4))
  #|
  (check-equal?: (map: + '(1 2 3) '(4 5 6))
                 : (Listof Number)
                 '(5 7 9))|#)






#|
(map: (compose F (curry map add1)) '((1 2) (3)))

Problem: in the code above, the input type of `F` has to be the return type of
`(curry map add1)`, i.e. `(Listof B)`. The return type of `F` may depend on its
input type (e.g. wrapping a value), so the type information flows leftwards
inside `compose`.

However, if F is a destructuring operation, like `car` or `cdr`, it may impose
constraints on the return type of the function immediately to its right, meaning
that the type information flows rightwards.

It seems difficult to reconcile these two cases without writing a complex
algorithm.

Worst-case scenario:

         +-- constrains to the right
         v                                  v-- constrains to the right
(compose car complex-calculation (curry map car))
             ^                   ^-- gives a (Listof ?) to the left
             +-- constrained on both sides

Maybe we could cover most common cases by first getting the type for the handled
cases which impose constraints to the right and/or give a type to the left, and
then use these types instead of the ∀, to fill in the holes for other functions.

EDIT: that's what we did, using the #:auto-in
|#



















#|
(define-for-syntax (map-infer-types stx)
  (syntax-parse stx
    [(_ (~literal car))
     (values #'(A B)
             #'(Pairof A B))]
    [(_ (~literal cdr)) #'(Pairof Any T)]
    [(_ T (~literal values)) #'T]
    [(_ T ((~literal compose))) #'T]
    [(_ T ((~literal compose) f0 . fs))
     #'(map-element (map-element T f0) (compose . fs))]
    [(_ T ((~literal curry) (~or (~literal map:) (~literal map)) f) l)
     #''_]
    ;; get
    [(_ f . ls)
     ;; TODO:
     #'T]))

(define-type-expander (map-element stx)
  (syntax-parse stx
    [(_ T:id (~literal car)) #'(Pairof T Any)]
    [(_ T:id (~literal cdr)) #'(Pairof Any T)]
    [(_ T (~literal values)) #'T]
    [(_ T ((~literal compose))) #'T]
    [(_ T ((~literal compose) f0 . fs))
     #'(map-element (map-element T f0) (compose . fs))]
    [(_ T ((~literal curry) (~or (~literal map:) (~literal map)) f) l)
     #''_]
    ;; get
    [(_ f . ls)
     ;; TODO:
     #'T]))


(define-type-expander (map-result stx)
  (syntax-parse stx
    [(_ T:id (~literal car)) #'T]
    [(_ T:id (~literal cdr)) #'T]))

(define-syntax (map: stx)
  (syntax-parse stx
    [(_ (~literal car) l) #'((curry-map A A (Pairof A Any) car) l)]
    [(_ (~literal cdr) l) #'((curry-map B B (Pairof Any B) cdr) l)]
    ;; TODO: add caar etc.
    [(_ ((~literal values)) l) #'l]
    [(_ ((~literal compose)) l) #'l]
    [(_ ((~literal compose) f0 . fs) l) #'(map: f0 (map: (compose . fs) l))]
    [(_ ((~literal curry) (~or (~literal map:) (~literal map)) f) l)
     #''_]
    [(_ ((~literal λget) field-or-accessor …) l)
     #'(get l (… …) field-or-accessor …)]
    [(_ f . ls)
     #'(map f . ls)]))



|#









#|

#;#'(let ()      
      (: map2 (∀ (poly-types …) (→ function-types …
                                   (Listof (Listof A))
                                   (Listof (Listof D)))))
      (define (map2 f … l)
        (if (null? l)
            '()
            (cons (map1 f … (car l))
                  (map2 f … (cdr l)))))
      (map2 f … l))

; (map: (curry map add1) '((1 2 3) (4 5))) =>
; (map: (curry map: add1) '((1 2 3) (4 5))) =>
(let ()
  (: map2 (∀ (A C) (→ (→ A C)
                      (Listof (Listof A))
                      (Listof (Listof C)))))
  (define (map2 f l)
    (if (null? l) '() (cons (map f (car l)) (map2 f (cdr l)))))
  (map2 add1 '((1 2 3) (4 5))))

;; TODO:
; (map: (compose (curry map (compose list add1))
;                (curry map (compose add1 add1)))
;       '((1 2 3) (4 5)))
; =>
#;???

; (map: (curry map (compose number->string add1)) '((1 2 3) (4 5))) =>
; (map: (curry map: (compose number->string add1)) '((1 2 3) (4 5))) =>
(let ()
  (: map2 (∀ (A C D) (→ (→ A C)
                        (→ C D)
                        (Listof (Listof A))
                        (Listof (Listof D)))))
  (define (map2 f g l)
    (if (null? l)
        '()
        (cons ;(map1 f g (car l))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (let ()
           (: map1 (∀ (A C D) (→ (→ A C)
                                 (→ C D)
                                 (Listof A)
                                 (Listof D))))
           (define (map1 f g l)
             (if (null? l) '() (cons (g (f (car l))) (map1 f g (cdr l)))))
           (map1 f g (car l)))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (map2 f g (cdr l)))))
  (map2 add1 number->string '((1 2 3) (4 5))))

; (map: add1 '(1 2 3))
(let ()
  (: map1 (∀ (A C) (→ (→ A C)
                      (Listof A)
                      (Listof C))))
  (define (map1 f l)
    (if (null? l) '() (cons (f (car l)) (map1 f (cdr l)))))
  (map1 add1 '(1 2 3)))

; (map: car '((1 a) (2 b) (3 c)))
(let ()
  (: map1 (∀ (A B) (→ (→ (Pairof A B) A)
                      (Listof (Pairof A B))
                      (Listof A))))
  (define (map1 f l)
    (if (null? l) '() (cons (f (car l)) (map1 f (cdr l)))))
  (map1 car
        '((1 a) (2 b) (3 c))))

; (map: (curry map car) '([(1 a) (2 b)] [(3 c)]))
; (map: (curry map: car) '([(1 a) (2 b)] [(3 c)]))
(let ()
  (: map1 (∀ (A B) (→ (→ (Pairof A B) A)
                      (Listof (Pairof A B))
                      (Listof A))))
  (define (map1 f l)
    (if (null? l) '() (cons (f (car l)) (map1 f (cdr l)))))
  
  (: map2 (∀ (A B) (→ (→ (Pairof A B) A)
                      (Listof (Listof (Pairof A B)))
                      (Listof (Listof A)))))
  (define (map2 f l)
    (if (null? l) '() (cons (map1 f (car l)) (map2 f (cdr l)))))
  
  (map2 car
        '([(1 a) (2 b)] [(3 c)])))

; (map: (curry map (curry map car)) '([(1 a) (2 b)] [(3 c)]))
; (map: (curry map (curry map: car)) '([(1 a) (2 b)] [(3 c)]))
; (map: (curry map: (curry map car)) '([(1 a) (2 b)] [(3 c)]))
; (map: (curry map: (curry map: car)) '([(1 a) (2 b)] [(3 c)]))
(let ()
  (: map3 (∀ (A B) (→ ;(→ (Pairof A B) A)
                    (Listof (Listof (Listof (Pairof A B))))
                    (Listof (Listof (Listof A))))))
  (define (map3 #|f|# l)
    (if (null? l)
        '()
        (cons (let ()
                (: map2 (∀ (A B) (→ ;(→ (Pairof A B) A)
                                  (Listof (Listof (Pairof A B)))
                                  (Listof (Listof A)))))
                (define (map2 #|f|# l)
                  (if (null? l)
                      '()
                      (cons (let ()
                              (: map1 (∀ (A B) (→ ;(→ (Pairof A B) A)
                                                (Listof (Pairof A B))
                                                (Listof A))))
                              (define (map1 #|f|# l)
                                (if (null? l)
                                    '()
                                    (cons (#|f|#car (car l))
                                          (map1 #|f|# (cdr l)))))
                              (map1 #|f|# (car l)))
                            (map2 #|f|# (cdr l)))))
                (map2 #|f|# (car l)))
              (map3 #|f|# (cdr l)))))
  (map3 ;car
   '([[(1 a) (2 b)] [(3 c)]] [[(4 d)]])))

;(define-syntax-rule (inst-∀ T …)


|#