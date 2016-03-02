#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide ? ?*)
  
  (define-syntax (?* stx)
    (syntax-case stx ()
      [(q . rest)
       (quasisyntax/loc stx
         ((λ () : (U) #,(syntax/loc #'q (error "Not implemented yet"))
            . rest)))]))
  
  (define-syntax (? stx)
    (syntax-case stx ()
      [(q t . rest)
       (quasisyntax/loc stx
         ((ann (λ () #,(syntax/loc #'q (error "Not implemented yet"))
                 . rest)
               (→ t))))])))