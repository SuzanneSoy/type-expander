#lang typed/racket

(module m typed/racket
  (define-syntax (m1 stx)
    (syntax-case stx ()
      [(_ (_ (e) _) b)
       (begin (displayln (free-identifier=? #'e #'b))
              #'(void))]))
  
  (define-syntax (frozen stx)
    (syntax-case stx ()
      [(_ def b)
       #`(begin def ;#,(datum->syntax #'a (syntax->datum #'(define def val)))
                (m1 def b))]))
  
  (define-syntax (goo stx)
    (syntax-case stx ()
      [(_ b)
       ;(begin (define i1 (make-syntax-delta-introducer #'te #'b))
       ;       (define i2 (make-syntax-delta-introducer #'b #'te))
       #`(frozen (define (te) 1)
                 #,(syntax-local-introduce #'b))]))
  
  (provide goo))

(require 'm)

(goo te)

#|

(define-syntax (lake stx)
  (syntax-parse stx
    [(_ val a)
     #`(let ((#,(datum->syntax stx 'tea) val)) a)]))

(lake 3 tea)

|#