#lang typed/racket

(require "../type-expander/type-expander.lp2.rkt"
         (for-syntax (submod "../type-expander/type-expander.lp2.rkt"
                             expander)))

(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ t)
     #`(begin
         (define-type-expander (xp stx)
           #'Number)
         (foo2 x t (Let (#,(syntax-local-introduce #'~>) xp) t))
         (define x 0))]))

(define-syntax (foo2 stx)
  (syntax-case stx ()
    [(_ x t u)
     (begin
       (let ((e (expand-type #'u)))
         (display "expanded:")
         (displayln e))
       #'(: x (U u Any)))]))

(foo (~> String))