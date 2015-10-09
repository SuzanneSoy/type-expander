#lang typed/racket
;(require mzlib/etc)
;(this-expression-file-name)

(provide define-to-this-file-name)

(define-syntax (define-to-this-file-name stx)
  (syntax-case stx ()
    [(_ name)
     #`(begin (define name #,(syntax-source #'dummy))
              (define-for-syntax name #,(syntax-source #'dummy)))]))

;(define-syntax (get-current-file stx)
;  #`(format "Macro in ~a, Use in ~a" structure.rkt-path #,(syntax-source stx)))

