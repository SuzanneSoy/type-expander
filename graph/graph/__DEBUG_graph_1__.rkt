#lang typed/racket

(module m typed/racket
  (define foo
    (let ()
      (define-type Foo (List 'foo (U Bar One)))
      (define-type Bar (List 'bar (U Foo Zero)))
      
      (ann (list 'foo (list 'bar 0)) Foo)))
  
  (provide foo))

(require 'm)

;(: f (∀ (A) (→ (List Symbol A) A)))
;(define (f x) (cadr x))

;(unpack (s-f foo))

(cadr foo)

#|
(module m typed/racket
  (struct (A) s ([f : A]))
  (define foo
    (let ()
      (define-type Foo (List 'foo (U (s Foo) (s Bar) One)))
      (define-type Bar (List 'bar (U (s Foo) (s Bar) Zero)))
      
      (ann (s (list 'foo 1)) (s Foo))))

  ;(: unpack (∀ (A) (→ (List Symbol A) A)))
  ;(define (unpack x) (cadr x))
  
  (provide (struct-out s)
           ;unpack
           foo))

(require 'm)

;(: f (∀ (A) (→ (List Symbol A) A)))
;(define (f x) (cadr x))

;(unpack (s-f foo))

(s-f foo)
|#