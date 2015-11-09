#lang typed/racket


#;(λ ([x : (U (Vector Number) (Vector String String))])
    (ann (vector-ref x 0) (U Number String)))

(ann (λ ([x : (U (Vector Number) (Vector String String) Symbol)])
       (if (vector? x)
           x
           #f))
     (→ (U (Vector Number) (Vector String String) Symbol)
        (U False (Vector Number) (Vector String String))))


(λ ([x : (U (→ Number Number Number) (→ Number Number))])
  (procedure-arity x))