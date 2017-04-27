#lang typed/racket
(require type-expander
         typed/rackunit
         (for-syntax racket/list))

(define-type-expander (Repeat stx)
  (syntax-case stx ()
    [(_ t n)
     #`(List #,@(map (λ (x) #'t)
                     (range (syntax->datum #'n))))]))

(: five-strings (→ String (Repeat String 5)))
(define (five-strings x)
  (list x "a" "b" "c" "d"))

(check-equal? (five-strings "hello")
              '("hello" "a" "b" "c" "d"))

(check-equal? (ann (five-strings "moon") (Repeat String 5))
              '("moon"  "a" "b" "c" "d"))
