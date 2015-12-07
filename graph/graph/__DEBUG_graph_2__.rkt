#lang typed/racket

(module m typed/racket
  (define-type (TFoo A B) (List 'foo (U (TFoo A B) (TBar A B) A)))
  (define-type (TBar A B) (List 'bar (U (TFoo A B) (TBar A B) B)))
  
  (define foo
    (let ()
      (define-type Foo (TFoo One Zero))
      (define-type Bar (TBar One Zero))

      (ann (list 'foo 1) Foo)))
  
  (provide foo))

(require 'm)

(: f (∀ (A) (→ (List Symbol A) Any)))
(define (f x) 2)

(f foo)