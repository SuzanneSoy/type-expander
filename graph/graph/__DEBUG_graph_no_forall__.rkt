#lang typed/racket

(module m typed/racket
  (define-values (x f)
    (let ()
      (define-type X (List (U X Null)))
      
      (define (f [x : X]) : Integer
        (if (null? (car x))
            0
            (add1 (f (car x)))))
      
      (define x : X (list (list (list '()))))
      
      (values x f)))
  
  (provide x f))

(require 'm)

(f x)