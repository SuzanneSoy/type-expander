#lang racket

(require typed/racket);(only-meta-in 0 typed/racket))

(provide (except-out (all-from-out typed/racket)
                     #%module-begin)
         (rename-out [module-begin #%module-begin]))

(require (for-syntax syntax/parse))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ forms ... ((~literal define-list-values) name rest ...) values ...)
     #'(#%module-begin (define-for-syntax name '(values ...))
                       (define name rest ... '(values ...))
                       forms ...)]))