#lang typed/racket

(require (for-syntax syntax/parse
                     "../lib/low-untyped.rkt"))

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
         #'(ann (λ (l) ((inst map Result-Type Element-Type) f l))
                (∀ (TVar) (→ (Listof Element-Type)
                             (Listof Result-Type))))
         #`(curry-map TVar
                      #,((attribute f.wrap) #'Result-Type
                                            (λ (t) #`(Listof #,t)))
                      #,((attribute f.wrap) #'Element-Type
                                            (λ (t) #`(Listof #,t)))
                      (curry-map TVar Result-Type Element-Type f.inner)))]))