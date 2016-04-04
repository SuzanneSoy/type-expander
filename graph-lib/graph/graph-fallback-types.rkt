#lang s-exp "../lib.rkt"

;; Tests with incomplete / outer-incomplete type-expander.

(define-type-expander (outer-incomplete stx)
  (syntax-case stx ()
    [(_ n)
     #;(raise-syntax-error
        'incomplete
        (format "Type doesn't have an incomplete counterpart: ~a"
                (syntax->datum #'n))
        #'n)
     ;; Just for testing:
     #''test]))

(define-type C Boolean)

(define-type C/incomplete (Pairof 'C Boolean))

(define-type-expander (incomplete stx)
  (syntax-case stx ()
    [(_ n)
     (cond [(free-identifier=? #'n #'C) #'C/incomplete]
           [else #'(outer-incomplete n)])]))

(let ()
  (define-type-expander (outer-incomplete stx)
    (syntax-case stx () [(_ n) #'(incomplete n)]))
  (let ()
    (define-type A Number)
    (define-type B String)
    
    (define-type A/incomplete (Pairof 'A Number))
    (define-type B/incomplete (Pairof 'B String))
    
    (define-type-expander (incomplete stx)
      (syntax-case stx ()
        [(_ n)
         (cond [(free-identifier=? #'n #'A) #'A/incomplete]
               [(free-identifier=? #'n #'B) #'B/incomplete]
               [else
                #'(outer-incomplete n)])]))
    
    (define-type TA A)
    (define-type TAI (incomplete A))
    (ann '(A . 1) TAI)
    
    (define-type TC C)
    (define-type TCI (incomplete C))
    (ann #t TC)
    (ann '(C . #t) TCI)
    
    (let ()
      (define-type A Boolean)
      (define-type TA A)
      (define-type TAI (incomplete A))
      (ann 'test TAI)
      (void))))