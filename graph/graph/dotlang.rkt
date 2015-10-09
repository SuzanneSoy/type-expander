#lang typed/racket

(module dotlang racket
  (require typed/racket)
  (provide (except-out (all-from-out typed/racket) #%top)
           (rename-out [new-#%top #%top]))
  
  (require (for-syntax racket/string))
  
  (define-syntax-rule (dot . xyz)
    '(dot . xyz))
  
  (define-syntax (new-#%top stx)
    (syntax-case stx ()
      [(_  . x)
       (let ([components (string-split (symbol->string (syntax->datum #'x))
                                       ".")])
         (if (> (length components) 1)
             #`(dot . #,components)
             #'(#%top . x)))])))

(module test (submod ".." dotlang)
  (require typed/rackunit)
  (let ((foo.bar 42))
    (check-equal? foo.bar 42))
  (check-equal? foo.bar '(dot "foo" "bar")))
