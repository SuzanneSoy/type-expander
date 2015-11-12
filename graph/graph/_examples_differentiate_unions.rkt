#lang typed/racket/no-check


#;(λ ([x : (U (Vector Number) (Vector String String))])
    (ann (vector-ref x 0) (U Number String)))

(let ()
  (ann (λ ([x : (U (Vector Number) (Vector String String) Symbol)])
         (if (vector? x)
             x
             #f))
       (→ (U (Vector Number) (Vector String String) Symbol)
          (U False (Vector Number) (Vector String String))))
  
  
  (λ ([x : (U (→ Number Number Number) (→ Number Number))])
    (procedure-arity x))
  (void))