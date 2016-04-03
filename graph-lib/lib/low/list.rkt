#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide indexof
           replace-first
           map+fold
           AListof)

  (define-type (AListof K V) (Listof (Pairof K V)))
  
  (: indexof (∀ (A B) (→ A (Listof B) (→ A B Any) (U #f Integer))))
  (define (indexof elt lst [compare equal?])
    (let rec ([lst lst] [index 0])
      (if (null? lst)
          #f
          (if (compare elt (car lst))
              index
              (rec (cdr lst) (+ index 1))))))
  
  (: replace-first (∀ (A B C) (->* (B
                                    C
                                    (Listof (U A B)))
                                   (#:equal? (→ (U A B) (U A B) Any : #:+ B))
                                   (Rec R (U (Pairof (U A B) R)
                                             Null
                                             (Pairof C (Listof (U A B))))))))
  (define (replace-first from to l #:equal? [equal? eq?])
    (if (null? l)
        '()
        (if (equal? from (car l))
            (cons to (cdr l))
            (cons (car l)
                  (replace-first from to (cdr l))))))
  
  (: map+fold (∀ (E R A) (→ (→ E A (values R A)) A (Listof E)
                            (Values (Listof R) A))))
  (define (map+fold f init-acc lst)
    (let ([result (foldl (λ ([item : E] [acc : (Pairof (Listof R) A)])
                           (let-values ([(item new-acc) (f item (cdr acc))])
                             (cons (cons item (car acc))
                                   new-acc)))
                         (cons '() init-acc)
                         lst)])
      (values (car result) (cdr result)))))