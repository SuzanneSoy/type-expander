#lang s-exp "../../lib.rkt"
(require (for-syntax racket/base))

(define-type ma (List (U ma Number) (U ma Number)) #:omit-define-syntaxes)
(define-multi-id ma
  #:match-expander (λ (stx) #'(list a b))
  #:call (λ (stx) #'(list 1 (list 2 3))))

(check-equal?:
 (match (ann (ma) ma)
   [(ma) #t])
 #t)