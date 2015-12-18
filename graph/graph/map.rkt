#lang typed/racket

(require (for-syntax syntax/parse)
         "../lib/low.rkt")

(provide map:)

(begin-for-syntax
  (define-syntax-class lam
    (pattern (~or (~literal λ) (~literal lambda)))))

(define-syntax-rule (map:: TVar Element-Type f l)
  ((λ #:∀ (TVar) ([lst : (Listof Element-Type)])
     ((inst map TVar Element-Type) f lst)) l))

(define-syntax (map: stx)
  (syntax-parse stx
    [(_ (~literal car) l) #'(map:: A (Pairof A Any) car l)]
    [(_ (~literal cdr) l) #'(map:: B (Pairof Any B) cdr l)]
    ;; TODO: add caar etc.
    [(_ ((~literal values)) l) #'l]
    [(_ ((~literal compose)) l) #'l]
    [(_ ((~literal compose) f0 . fs) l) #'(map: f0 (map: (compose . fs) l))]
    [(_ f . ls)
     #'(map f . ls)]))

(module* test typed/racket
  (require (submod "..")
           "../lib/low.rkt")
  
  (check-equal?: (map: car '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: cdr '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '((b x) (c) (d)))
  (check-equal?: (map: car (map: cdr '((1 b x) (2 c) (3 d))))
                 : (Listof Number)
                 '(b c d))
  (check-equal?: (map: (compose) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '((1 b x) (2 c) (3 d)))
  (check-equal?: (map: (compose car) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(1 2 3))
  (check-equal?: (map: (compose cdr) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '((b x) (c) (d)))
  (check-equal?: (map: (compose car cdr) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(b c d))
  (check-equal?: (map: (compose add1 car) '((1 b x) (2 c) (3 d)))
                 : (Listof Number)
                 '(2 3 4))
  (check-equal?: (map: + '(1 2 3) '(4 5 6))
                 : (Listof Number)
                 '(5 7 9)))