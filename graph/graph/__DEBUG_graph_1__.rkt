#lang typed/racket

#|
(module m typed/racket
  (define foo
    (let ()
      (define-type Foo (Pairof (Promise Foo) Number))
      
      (define (f [n : Number]) : Foo
        (cons (delay (f (add1 n)))
              n))
      (f 1)))
  
  (provide foo))

(require 'm)
(force (car foo))
|#

(define-type (P A) (Pairof (U A Null) (U A Null)))

(define bar
  (let ()
    (define-type Bar (Pairof (P Bar) Number))

    (define (f [n : Number]) : Bar
      (cons (ann (cons '() '()) (P Bar))
            n))
    
    (f 1)))

(provide P bar)

#|
(define foo
  (let ()
    ;(define-type Foo (Rec F (Pairof (Promise F) Number)))
    (define-type Foo (Pairof (Promise Foo) Number))
    
    (define (f [n : Number]) : Foo
      (cons (delay (f (add1 n)))
            n))
    (f 1)))

(provide foo)
|#