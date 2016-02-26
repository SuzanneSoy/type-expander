#lang typed/racket

(require "__DEBUG_graph6G-req.rkt")

(module m typed/racket
  (require "__DEBUG_graph6G-req.rkt"
           macro-debugger/syntax-browser
           (for-syntax syntax/parse)
           (for-syntax syntax/parse/experimental/template))
  
  (define-syntax (m1 stx)
    (syntax-parse stx
      [(_ m2-id (~and code (_ _ (~and (~datum foo) su))))
       #`(begin code
                #,(free-identifier=? #'m2-id #'su))]))
  
  (define-syntax (rich-graph stx)
    (syntax-parse stx
      [(_ user-code)
       (define i (make-syntax-introducer))
       
       #`(begin
           #,(i #'(define-type-expander (foo stx) #'Number))
           (m1 foo #,(i #'user-code)))]))
  
  (provide rich-graph))

(require 'm)

(let ((y 1))
  (rich-graph (ann y foo)))
(let ((y 2))
  (rich-graph (ann y foo)))
