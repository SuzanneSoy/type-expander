#lang typed/racket

(module test racket
  (require (for-syntax "meta-struct.rkt")
           rackunit)
  
  (define-syntax (test-subtype? stx)
    (syntax-case stx ()
      [(_ sub super)
       #`#,(if (meta-struct-subtype? #'sub #'super)
               #t
               #f)]))
  
  (module m1 racket
    (struct sa ())
    (provide (struct-out sa)))
  (module m2 racket
    (require (submod ".." m1))
    (struct sb sa ())
    (provide (rename-out [sa sa2]))
    (provide (struct-out sb)))
  (require 'm1)
  (require 'm2)
  (struct sc sb ())
  
  (check-true (test-subtype? sa sa))
  (check-true (test-subtype? sa2 sa))
  (check-true (test-subtype? sb sa))
  (check-true (test-subtype? sc sa))
  
  (check-true (test-subtype? sa sa2))
  (check-true (test-subtype? sa2 sa2))
  (check-true (test-subtype? sb sa2))
  (check-true (test-subtype? sc sa2))
  
  (check-false (test-subtype? sa sb))
  (check-false (test-subtype? sa2 sb))
  (check-true (test-subtype? sb sb))
  (check-true (test-subtype? sc sb))
  
  (check-false (test-subtype? sa sc))
  (check-false (test-subtype? sa2 sc))
  (check-false (test-subtype? sb sc))
  (check-true (test-subtype? sc sc)))