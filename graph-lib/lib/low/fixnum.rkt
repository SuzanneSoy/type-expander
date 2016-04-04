#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules
  (provide fxxor)
  
  ;; For fxxor, used to compute hashes.
  ;; The type obtained just by writing (require racket/fixnum) is wrong, so we
  ;; get a more precise one.
  (require/typed racket/fixnum [(fxxor fxxor2) (→ Fixnum Fixnum Fixnum)])
  
  (: fxxor (→ Fixnum * Fixnum))
  (define (fxxor . args)
    (foldl fxxor2 0 args))
  
  (module+ test
    (require typed/rackunit)
    (check-equal? (fxxor2 13206 23715) 28469)
    (check-equal? (fxxor 0) 0)
    (check-equal? (fxxor 13206) 13206)
    (check-equal? (fxxor 13206 23715 314576) 304101)))