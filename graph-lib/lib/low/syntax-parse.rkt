#lang typed/racket
(require "typed-untyped.rkt")

(module m-stx-identifier racket
  (require racket/stxparam)
  
  (provide stx)
  
  (define-syntax-parameter stx
    (lambda (call-stx)
      (raise-syntax-error
       'stx
       "Can only be used in define-syntax/parse or λ/syntax-parse"
       call-stx))))

(define-typed/untyped-modules #:no-test
  (provide stx
           define-syntax/parse
           λ/syntax-parse
           ~maybe
           ~maybe*
           ~optkw
           ~kw
           ~lit
           ~or-bug
           define-simple-macro
           λstx
           ;template/loc
           ;quasitemplate/loc
           template/debug
           quasitemplate/debug
           meta-eval)
  (begin-for-syntax
    (provide stx))
  
  (require syntax/parse
           syntax/parse/define
           syntax/parse/experimental/template
           (for-syntax racket/syntax
                       racket/stxparam)
           (for-meta 2 racket/base racket/syntax)
           racket/stxparam)
  
  ;(require "typed-untyped.rkt")
  ;(require-typed/untyped "backtrace.rkt")
  (require (for-syntax "backtrace.rkt")
           "backtrace.rkt")
  
  (define-syntax ~maybe
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(self pat ...)
          (define (s stx) (datum->syntax #'self stx stx stx))
          #`(#,(s #'~optional) (#,(s #'~seq) pat ...))]))))
  
  (define-syntax ~maybe*
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(self name pat ...)
          (define (s stx) (datum->syntax #'self stx stx stx))
          #`(#,(s #'~and) name (#,(s #'~optional) (#,(s #'~seq) pat ...)))]))))
  
  (define-syntax ~optkw
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(self kw:keyword)
          (define (s stx) (datum->syntax #'self stx stx stx))
          (define/with-syntax name
            (format-id #'kw "~a" (keyword->string (syntax-e #'kw))))
          #`(#,(s #'~optional) (#,(s #'~and) name kw))]))))
  
  (define-syntax ~kw
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(self kw:keyword)
          (define (s stx) (datum->syntax #'self stx stx stx))
          (define/with-syntax name
            (format-id #'kw "~a" (keyword->string (syntax-e #'kw))))
          #`(#,(s #'~and) name kw)]))))
  
  ;; Circumvent the bug that causes "syntax-parse: duplicate attribute in: a" in
  ;; (syntax-parse #'(x y z) [((~or a (a b c)) ...) #'(a ...)])
  (define-syntax ~or-bug
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(self pat ...)
          (define (s stx) (datum->syntax #'self stx stx stx))
          #`(#,(s #'~and) x (#,(s #'~parse) (#,(s #'~or) pat ...) #'x))]))))
  
  (define-syntax ~lit
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(self (~optional (~seq name:id (~literal ~))) lit)
          (define (s stx) (datum->syntax #'self stx stx stx))
          (if (attribute name)
              #`(#,(s #'~and) name (#,(s #'~literal) lit))
              #`(#,(s #'~literal) lit))]
         [(self (~optional (~seq name:id (~literal ~))) lit …)
          (define (s stx) (datum->syntax #'self stx stx stx))
          (if (attribute name)
              #`(#,(s #'~and) name (#,(s #'~seq) (#,(s #'~literal) lit)))
              #`(#,(s #'~seq) (#,(s #'~literal) lit)))]))))
  
  (require (submod ".." m-stx-identifier)
           (for-syntax (submod ".." m-stx-identifier)))
  
  (define-simple-macro (define-syntax/parse (name . args) body0 . body)
    (define-syntax (name stx2)
      (with-backtrace (syntax->datum stx2)
        (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
          (syntax-parse stx2
            [(_ . args) body0 . body])))))
  
  (define-simple-macro (λ/syntax-parse args . body)
    (λ (stx2)
      (with-backtrace (syntax->datum stx2)
        (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
          (syntax-parse stx2
            [args . body])))))
  
  ;; λstx
  (begin
    (define-syntax-rule (λstx (param ...) body ...)
      (λ (param ...)
        (with-syntax ([param param] ...)
          body ...)))
    
    (module+ test
      (require typed/rackunit)
      (check-equal? (syntax->datum ((λstx (foo bar) #'(foo bar)) #'a #'b))
                    (syntax->datum #'(a b)))))
  
  ;; template/loc
  (begin
    (define-syntax-rule (template/loc loc . tmpl)
      (quasisyntax/loc loc #,(template . tmpl))))

  ;; quasitemplate/loc
  (begin
    (define-syntax-rule (quasitemplate/loc loc . tmpl)
      (quasisyntax/loc loc #,(quasitemplate . tmpl))))
  
  ;; template/debug
  (begin
    (define-syntax (template/debug stx)
      (syntax-parse stx
        [(_ debug-attribute:id . rest)
         #'((λ (x)
              (when (attribute debug-attribute)
                (pretty-write (syntax->datum x)))
              x)
            (template . rest))])))
  
  ;; quasitemplate/debug
  (begin
    (define-syntax (quasitemplate/debug stx)
      (syntax-parse stx
        [(_ debug-attribute:id . rest)
         #'((λ (x)
              (when (attribute debug-attribute)
                (pretty-write (syntax->datum x)))
              x)
            (quasitemplate . rest))])))
  
  ;; meta-eval
  (begin
    ;; TODO: this is kind of a hack, as we have to write:
    #;(with-syntax ([(x …) #'(a bb ccc)])
        (let ([y 70])
          (quasitemplate
           ([x (meta-eval (+ #,y (string-length
                                  (symbol->string
                                   (syntax-e #'x)))))]
            …))))
    ;; Where we need #,y instead of using:
    ;; (+ y (string-length etc.)).
    (module m-meta-eval racket
      (provide meta-eval)
      (require syntax/parse/experimental/template)
      
      (define-template-metafunction (meta-eval stx)
        (syntax-case stx ()
          [(_ . body)
           #`#,(eval #'(begin . body))])))
    (require 'm-meta-eval)))