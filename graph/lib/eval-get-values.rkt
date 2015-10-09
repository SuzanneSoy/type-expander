#lang typed/racket

(module m racket
  (provide eval-get-values)
  
  (define (eval-get-values expr namespace)
    (call-with-values (λ () (eval expr namespace)) list)))

(require/typed 'm [eval-get-values (→ Any Namespace (Listof Any))])

(provide eval-get-values)
