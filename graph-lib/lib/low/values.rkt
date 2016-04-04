#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules
  (provide first-value second-value third-value fourth-value fifth-value
           sixth-value seventh-value eighth-value ninth-value tenth-value
           cons→values
           (rename-out [cons→values cons->values]))
  
  (define-syntax-rule (define-value-getter name v ... last-v)
    (define-syntax-rule (name expr)
      (call-with-values (λ () expr) (λ (v ... last-v . rest) last-v))))
  
  (define-value-getter first-value   v1)
  (define-value-getter second-value  v1 v2)
  (define-value-getter third-value   v1 v2 v3)
  (define-value-getter fourth-value  v1 v2 v3 v4)
  (define-value-getter fifth-value   v1 v2 v3 v4 v5)
  (define-value-getter sixth-value   v1 v2 v3 v4 v5 v6)
  (define-value-getter seventh-value v1 v2 v3 v4 v5 v6 v7)
  (define-value-getter eighth-value  v1 v2 v3 v4 v5 v6 v7 v8)
  (define-value-getter ninth-value   v1 v2 v3 v4 v5 v6 v7 v8 v9)
  (define-value-getter tenth-value   v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)
  
  (module+ test
    (require typed/rackunit)
    (check-equal? (first-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 1)
    (check-equal? (second-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 2)
    (check-equal? (third-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 3)
    (check-equal? (fourth-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 4)
    (check-equal? (fifth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 5)
    (check-equal? (sixth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 6)
    (check-equal? (seventh-value (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 7)
    (check-equal? (eighth-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 8)
    (check-equal? (ninth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 9)
    (check-equal? (tenth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 10))
  
  (define #:∀ (A B) (cons→values [x : (Pairof A B)]) (values (car x) (cdr x))))