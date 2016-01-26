#lang typed/racket

(require (for-syntax racket/syntax
                     syntax/stx
                     syntax/parse
                     syntax/parse/experimental/template
                     "../lib/low-untyped.rkt")
         "../lib/low.rkt"
         "get.lp2.rkt"
         "../type-expander/type-expander.lp2.rkt")

;; TODO: DEBUG
(define-syntax (dbg stx)
  (syntax-parse stx
    [(_ (~optional (~and norun #:norun)) code)
     (if (attribute norun)
         #'(ann 'code Any)
         #'code)]))

(begin-for-syntax
  (define-template-metafunction (apply-compose stx)
    (syntax-parse stx
      [(_ [] [a])
       #'a]
      [(_ [] [a …])
       #'(values a …)]
      [(_ [f … (~and loc (f-last x … (~literal _) y …))] [a …])
       (quasitemplate
        (apply-compose [f …] [#,(syntax/loc #'loc (f-last x … a … y …))]))]
      [(_ [f … f-last] [a …])
       (quasitemplate
        (apply-compose [f …] [#,(syntax/loc #'f-last (f-last a …))]))])))

(begin-for-syntax
  (define-syntax-class fun-descriptor
    (pattern [in:expr f-auto:expr out:expr]
             #:with code (generate-temporary #'f-auto)
             #:with (f-arg …) #'(f-auto)
             #:with (f-param …) #'(code)
             #:with (f-type …) #'[(→ in out)])
    (pattern [in:expr
              {[f-arg:expr f-param:id f-type:expr] …}
              code:expr
              out:expr])))

(define-syntax (map0 stx)
  (syntax-parse stx
    [(_ (~optional (~and norun #:norun))
        #:∀ {∀-T:id …} global-out:expr
        :fun-descriptor …)
     (define/with-syntax local-map (generate-temporary 'map))
     (define/with-syntax lst (generate-temporary 'lst))
     (define/with-syntax (_ … inner-in) #'(in …))
     (define/with-syntax icons #'(inst cons global-out (Listof global-out)))
     (quasitemplate
      (dbg (?? norun)
           (let ()
             (: local-map (∀ (∀-T …) (→ f-type … …
                                        (Listof inner-in)
                                        (Listof global-out))))
             (define (local-map f-param … … lst)
               (if (null? lst)
                   '()
                   (icons (apply-compose [code …] [(car lst)])
                          (local-map f-param … … (cdr lst)))))
             local-map)))]))

(define-syntax (call-map0 stx)
  (syntax-parse stx
    [(_ (~optional (~and norun #:norun))
        l:expr #:∀ {∀-T:id …} global-out:expr d:fun-descriptor …)
     (quasitemplate
      (dbg (?? norun)
           #,(syntax/loc stx
               ((map0 #:∀ {∀-T …} global-out d …) d.f-arg … … l))))]))

(call-map0 #:norun '((a 1) (b 2) (c 3))
           #:∀ {O}
           O
           [(Pairof O Any) car O]
           [(Pairof Any (Pairof O Any)) cdr (Pairof O Any)])

(call-map0 #:norun '(1 2 3)
           #:∀ {A B}
           B
           [A add1 B]
           [A identity A])

(call-map0 #:norun '(1 2 3)
           #:∀ {A B}
           B
           [A add1 B]
           [A identity A])

(call-map0 '((1 2) (3)) #:∀ {A B}
           (Listof B)
           [(Listof A) {[add1 add1*1 (→ A B)]}
                       (call-map0 _ #:∀ {A B} B [A add1*1 B])
                       (Listof B)])

(call-map0 '((1 2) (3)) #:∀ {A B C}
           (Listof C)
           [(Listof B) {[add1 add1*2 (→ B C)]}
                       (call-map0 _ #:∀ {B C} C [B add1*2 C])
                       (Listof C)]
           [(Listof A) {[add1 add1*1 (→ A B)]}
                       (call-map0 _ #:∀ {A B} B [A add1*1 B])
                       (Listof B)])

(call-map0 '((1 2) (3)) #:∀ {A Y B X C}
           (Listof C)
           [(Listof B) {[add1 add1*2 (→ B X)]
                        [add1 add1*2b (→ X C)]}
                       (call-map0 _ #:∀ {B X C} C
                                  [X add1*2b C]
                                  [B add1*2 X])
                       (Listof C)]
           [(Listof A) {[add1 add1*1 (→ A Y)]
                        [add1 add1*1b (→ Y B)]}
                       (call-map0 _ #:∀ {A Y B} B
                                  [Y add1*1b B]
                                  [A add1*1 Y])
                       (Listof B)])

(call-map0 '(([1 b] [2 c]) ([3 d])) #:∀ {A X C}
           (Listof C)
           [(Listof A) {[add1 add1*2 (→ A X)]
                        [add1 add1*2b (→ X C)]}
                       (call-map0 _ #:∀ {A X C} C
                                  [X add1*2b C]
                                  [A add1*2 X])
                       (Listof C)]
           [(Listof (Pairof A Any)) {}
                                    (call-map0 _ #:∀ {A} A
                                               [(Pairof A Any) car A])
                                    (Listof A)])

#;(call-map0 '(([1 b] [2 c]) ([3 d])) #:∀ {A X C}
           (Listof C)
           [(Listof (Pairof A Any))
            {[add1 add1*2b (→ A C)]}
            (call-map0 _ #:∀ {A C} C
                       [A add1*2b C]
                       [(Pairof A Any) car A])
            (Listof C)]
           [(Listof (Pairof (Pairof A) Any))
            {}
            (call-map0 _ #:∀ {A} A
                       [(Pairof (Pairof A Any) Any) car A])
            (Listof (Pairof A Any))])