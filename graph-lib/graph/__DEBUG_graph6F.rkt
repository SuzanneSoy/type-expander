#lang racket

(module m racket
  (require macro-debugger/syntax-browser)
  (define-syntax (m1 stx)
    (syntax-case stx ()
      [(_ sol (su sv) m2-id user-id i-user-id a f r aa ff rr)
       (syntax-local-lift-expression
        #`(browse-syntaxes
           (list #'sol #'m2-id #'user-id #'i-user-id #'a #'f #'r #'aa #'ff #'rr)))
       #`(cons (list (su) sv) #,(free-identifier=? #'m2-id #'sol))]))
  
  (define-syntax (m2 stx)
    (syntax-case stx ()
      [(_ user-id val)
       #`(begin
           (define (foo) 1)
           (m1
            ;#,((make-syntax-delta-introducer #'foo #'user-id) (syntax-local-introduce #'user-id) 'add)
            #,((make-syntax-delta-introducer #'foo stx) (syntax-local-introduce #'user-id) 'add)
            #,((make-syntax-delta-introducer #'foo stx) (syntax-local-introduce #'(user-id val)) 'add)
            foo
            user-id
            #,(syntax-local-introduce #'user-id)
            #,((make-syntax-delta-introducer #'foo #'user-id) #'user-id 'add)
            #,((make-syntax-delta-introducer #'foo #'user-id) #'user-id 'flip)
            #,((make-syntax-delta-introducer #'foo #'user-id) #'user-id 'remove)
            #,((make-syntax-delta-introducer #'user-id #'foo) #'user-id 'add)
            #,((make-syntax-delta-introducer #'user-id #'foo) #'user-id 'flip)
            #,((make-syntax-delta-introducer #'user-id #'foo) #'user-id 'remove)))]))
  
  (provide m2))(require 'm)

(let ((y 1))
  (m2 foo y))
(let ((y 2))
  (m2 foo y))
