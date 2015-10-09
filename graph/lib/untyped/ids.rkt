#lang racket

(provide format-ids
         hyphen-ids
         format-temp-ids
         #|t/gen-temp|#)

(require racket/syntax) ;; Used to bind format-id in macroexpansion below.
(require racket/sequence) ;; Used to bind in-syntax in macroexpansion below.
;; Used to bind in-syntax on older versions in macroexpansion below:
;(require unstable/sequence)
(require (for-syntax racket/syntax
                     syntax/parse
                     racket/string
                     racket/sequence
                     ;unstable/sequence ;; in-syntax on older versions
                     #|syntax/parse/experimental/template|#)
         syntax/strip-context)

;; Actually, this could be just a regular function:
;; test (with list?, syntax? and combinations thereof) if we should iterate or
;; just put the value as-is.
(begin-for-syntax
  (define-syntax-class var-expr
    #:description
    (string-append "#'identifier or #'(identifier ooo), where ooo is a"
                   " literal “...”, or #'(identifier ...), or an expression")
    (pattern (~and whole ((~literal syntax) var:id))
             #:with code #'(in-value whole))
    (pattern (~and whole ((~literal syntax) (var:id (~literal ...))))
             #:with code #'(in-syntax whole))
    (pattern (~and whole ((~literal syntax) (vars:id ...)))
             #:with (var . _) #`(vars ... #,(gensym 'empty))
             #:with code #'(in-syntax whole))
    (pattern expr:expr
             #:with var (gensym)
             #:with code #'(let ((s expr)) (if (string? s) (in-value s) s)))))

;; TODO: infinite loop if we only have constants which ar handled with for-value
(define-syntax (format-ids stx)
  (syntax-parse stx
    [(_ lexical-context:expr format:expr v:var-expr ...)
     (define/with-syntax (tmp ...) (generate-temporaries #'(v.var ...)))
     #'(let ([lex-ctx lexical-context])
         (for/list ([tmp v.code] ...)
           (format-id (if (procedure? lex-ctx) (lex-ctx tmp ...) lex-ctx)
                      format
                      tmp ...)))]))

(define-syntax (hyphen-ids stx)
  (syntax-parse stx
    ;; TODO: allow single #'foo instead of (var expr), and use in-value
    [(_ lexical-context:expr v:var-expr ...)
     #`(format-ids lexical-context
                   #,(string-join (for/list ([x (in-syntax #'(v ...))])  "~a")
                                  "-")
                   v ...)]))

(define-syntax (format-temp-ids stx)
  (syntax-parse stx
    [(_ . rest)
     ;; Introduce the binding in a fresh scope.
     #'(format-ids (λ _ ((make-syntax-introducer) #'())) . rest)]))

#|
(define-template-metafunction (t/gen-temp stx)
  (syntax-parse stx
    [(_ . id:id)
     #:with (temp) (generate-temporaries #'(id))
     #'temp]
    [(_ id:id ...)
     (generate-temporaries #'(id ...))]))
|#

(module* test racket
  (require (submod "..")
           rackunit
           (for-syntax racket/syntax
                       (submod "..")))
  
  (check-equal? (format-ids #'a "~a-~a" #'() #'())
                '())
  (check-equal? (map syntax->datum
                     (format-ids #'a "~a-~a" #'(x1 x2 x3) #'(a b c)))
                '(x1-a x2-b x3-c))
  
  (define-syntax (test1 stx)
    (syntax-case stx ()
      [(_ (let1 d1) x y)
       (begin
         (define/with-syntax (foo-x foo-y)
           (format-ids (λ (xy)
                         (if (string=? (symbol->string (syntax->datum xy))
                                       "b")
                             stx
                             #'()))
                       "foo-~a"
                       #'(x y)))
         #'(let1 d1 (let ((foo-b 2) (foo-c 'b)) (cons foo-x foo-y))))]))

  (check-equal? (test1 (let ((foo-b 1) (foo-c 'a))) b c)
                '(1 . b))
  
  (define-syntax (fubar stx)
    (define/with-syntax (v1 ...) #'(1 2 3))
    (define/with-syntax (v2 ...) #'('a 'b 'c))
    ;; the resulting ab and ab should be distinct identifiers:
    (define/with-syntax (id1 ...) (format-temp-ids "~a" #'(ab cd ab)))
    (define/with-syntax (id2 ...) (format-temp-ids "~a" #'(ab cd ab)))
    #'(let ([id1 v1] ...)
        (let ([id2 v2] ...)
          (list (cons id1 id2) ...))))
  
  (check-equal? (fubar) '((1 . a) (2 . b) (3 . c))))
