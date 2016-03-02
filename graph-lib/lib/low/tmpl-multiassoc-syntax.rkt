#lang typed/racket
(require "../low2/typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide tmpl-cdr-assoc-syntax
           (rename-out [tmpl-cdr-assoc-syntax !cdr-assoc]))
  
  (require "../low2/typed-untyped.rkt")
  
  (module m-tmpl-cdr-assoc-syntax racket
    (provide tmpl-cdr-assoc-syntax)
    
    (require syntax/parse
             syntax/parse/experimental/template
             (submod "../low2/stx.rkt" untyped)
             (submod "multiassoc-syntax.rkt" untyped))
    
    (define-template-metafunction (tmpl-cdr-assoc-syntax stx)
      (syntax-parse stx
        [(_ (~optional (~seq #:default default)) query [k . v] …)
         (if (attribute default)
             (let ([r (assoc-syntax #'query #'([k . v] …))])
               (if r
                   (stx-cdr r)
                   #'default))
             (cdr-assoc-syntax #'query #'([k . v] …)))])))
  (require 'm-tmpl-cdr-assoc-syntax))