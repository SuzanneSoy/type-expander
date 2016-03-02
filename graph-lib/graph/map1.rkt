#lang typed/racket

(require (for-syntax syntax/parse
                     (submod "../lib/low.rkt" untyped))
         "../type-expander/type-expander.lp2.rkt")

(provide curry-map)

(begin-for-syntax
  (define-syntax-class curry-map-rec
    #:attributes (inner bottom bottom? wrap)
    (pattern ((~lit curry) (~lit map) inner:curry-map-rec)
             #:attr wrap (λ (x w) (w ((attribute inner.wrap) x w)))
             #:attr bottom #'inner.bottom
             #:attr bottom? #f)
    (pattern f
             #:attr wrap (λ (x w) x)
             #:attr bottom #'f
             #:attr bottom? #t
             #:attr inner #f)))

(define-syntax (curry-map stx)
  (syntax-parse stx
    [(_ TVar Result-Type Element-Type f:curry-map-rec)
     (if (attribute f.bottom?)
         ;; We use (ann λ type) instead of (λ #:∀ …) because as of version
         ;; 6.3.0.8--2015-12-17(0d633fe/a), the latter doesn't work if put in a
         ;; let's binding clause: (let ([f (λ #:∀ …)]) f) fails to typecheck.
         #'(ann (λ (l) ((inst map Result-Type Element-Type) f l))
                (∀ (TVar) (→ (Listof Element-Type)
                             (Listof Result-Type))))
         #`(curry-map TVar
                      #,((attribute f.wrap) #'Result-Type
                                            (λ (t) #`(Listof #,t)))
                      #,((attribute f.wrap) #'Element-Type
                                            (λ (t) #`(Listof #,t)))
                      (curry-map TVar Result-Type Element-Type f.inner)))]))