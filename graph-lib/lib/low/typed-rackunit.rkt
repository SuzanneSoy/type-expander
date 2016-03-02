#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  ;; TODO: these won't expand types in the ann.
  (provide check-equal?:
           check-not-equal?:
           check-ann)
  
  (require "typed-untyped.rkt")
  
  (require/typed rackunit
                 [(check-true untyped:check-true)
                  (->* (Any) (String) Any)]
                 [#:struct check-info ([name : Symbol] [value : Any])]
                 [make-check-info (→ Symbol Any check-info)]
                 [make-check-location (→ (List Any
                                               (U Number False)
                                               (U Number False)
                                               (U Number False)
                                               (U Number False))
                                         check-info)]
                 [make-check-name (→ Any check-info)]
                 [make-check-params (→ Any check-info)]
                 [make-check-actual (→ Any check-info)]
                 [make-check-expected (→ Any check-info)]
                 [make-check-expression (→ Any check-info)]
                 [make-check-message (→ Any check-info)]
                 [with-check-info* (→ (Listof check-info) (→ Any) Any)])
  
  (require (for-syntax syntax/parse
                       syntax/parse/experimental/template))
  (require-typed/untyped "syntax-parse.rkt")
  
  (define-syntax/parse
    (check-equal?: actual
                   (~optional (~seq (~datum :) type))
                   expected
                   (~optional message:expr))
    (quasitemplate
     (with-check-info* (list (make-check-actual (format "~s" actual))
                             (make-check-expected (format "~s" expected))
                             (make-check-name 'check-equal?:)
                             (make-check-params
                              (format "~s" `(,actual (?? 'type) ,expected)))
                             (make-check-location '(#,(syntax-source stx)
                                                    #,(syntax-line stx)
                                                    #,(syntax-column stx)
                                                    #,(syntax-position stx)
                                                    #,(syntax-span stx)))
                             (make-check-expression '#,(syntax->datum stx)))
                       (λ ()
                         (untyped:check-true
                          (equal? (?? (ann actual type) actual)
                                  expected))))))
  
  (define-syntax/parse
    (check-not-equal?: actual
                       (~optional (~seq (~datum :) type))
                       expected
                       (~optional message))
    (quasitemplate
     (with-check-info* (list (make-check-actual (format "~s" actual))
                             (make-check-expected (format "~s" expected))
                             (make-check-name 'check-not-equal?:)
                             (make-check-params
                              (format "~s" `(,actual (?? 'type) ,expected)))
                             (make-check-location '(#,(syntax-source stx)
                                                    #,(syntax-line stx)
                                                    #,(syntax-column stx)
                                                    #,(syntax-position stx)
                                                    #,(syntax-span stx)))
                             (make-check-expression '#,(syntax->datum stx)))
                       (λ ()
                         (untyped:check-true
                          (not (equal? (?? (ann actual type) actual)
                                       expected)))))))
  
  (define-syntax/parse (check-ann value type (~optional message))
    (quasitemplate
     ((λ _ (void)) (ann value type))
     #;(let ([value-cache value])
       (with-check-info* (list (make-check-actual (format "~s" value-cache))
                               (make-check-expected (format "~s" value-cache))
                               (make-check-name 'check-ann)
                               (make-check-params (format "~s" `(,value-cache
                                                                 type)))
                               (make-check-location '(#,(syntax-source stx)
                                                      #,(syntax-line stx)
                                                      #,(syntax-column stx)
                                                      #,(syntax-position stx)
                                                      #,(syntax-span stx)))
                               (make-check-expression '#,(syntax->datum stx)))
                         (λ ()
                           (untyped:check-true
                            (equal? (ann value type) value))))))))