#lang racket

;; Don't forget to require this at the template level! Otherwise, Racket will
;; complain that two instances of racket/gui are started.

(module m-browse-syntax typed/racket
  (require/typed macro-debugger/syntax-browser
                 [browse-syntax (→ Syntax Any)]
                 [browse-syntaxes (→ (Listof Syntax) Any)])
  
  (provide browse-syntax
           browse-syntaxes))

(define (debug-syntax stx)
  (syntax-local-lift-expression #`(browse-syntax #'#,stx)))

(require 'm-browse-syntax)
(provide browse-syntax
         browse-syntaxes
         debug-syntax)