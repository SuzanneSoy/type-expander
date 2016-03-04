#lang racket

(provide ;typed/untyped
 require-typed/untyped-typed
 require-typed/untyped
 require/provide-typed/untyped
 define-typed/untyped-modules
 if-typed
 when-typed
 when-untyped)

(require typed/untyped-utils
         racket/require-syntax
         (for-syntax syntax/parse
                     racket/syntax
                     syntax/stx
                     syntax/strip-context))

(module m-typed typed/racket
  (provide (rename-out [require tr:require]
                       [provide tr:provide])
           ;typed/untyped
           #;require-typed/untyped)
  
  #;(require (for-syntax syntax/parse
                         racket/syntax
                         syntax/stx
                         syntax/strip-context)
             racket/require-syntax)
  
  
  
  #;(define-syntax (require-typed/untyped stx)
      (syntax-case stx ()
        [(_ m)
         (let ()
           (define/with-syntax sb (datum->syntax #'m 'submod #'m #'m))
           (define/with-syntax ty (datum->syntax #'m 'typed #'m #'m))
           #'(require (sb m ty)))])))

#;(require 'm-typed)

;; require
(define-syntax (require-typed/untyped-typed stx)
  (syntax-parse stx
    [(_ . (~and ms (m ...)))
     (replace-context #'ms #'(require (submod m typed) ...))]))

#;(define-require-syntax (typed/untyped-typed stx)
    (syntax-case stx ()
      [(_ m) (replace-context stx #'(submod m typed))]))

#;(define-require-syntax (typed/untyped-untyped stx)
    (syntax-case stx ()
      [(_ m) (replace-context stx #'(submod m untyped))]))

(define-syntax (require-typed/untyped-untyped stx)
  (syntax-parse stx
    [(_ . (~and ms (m ...)))
     (replace-context #'ms #'(require (submod m untyped) ...))]))

(define-typed/untyped-identifier require-typed/untyped
  require-typed/untyped-typed
  require-typed/untyped-untyped)

#;(define-typed/untyped-identifier typed/untyped
    typed/untyped-typed
    typed/untyped-untyped)

;; require/provide
;; TODO: make a require expander instead.
(define-syntax (require/provide-typed/untyped-typed stx)
  (syntax-parse stx
    [(_ . (~and ms (m ...)))
     (replace-context #'ms
                      #'(begin
                          (require (submod m typed) ...)
                          (provide (all-from-out (submod m typed) ...))))]))

(define-syntax (require/provide-typed/untyped-untyped stx)
  (syntax-parse stx
    [(_ . (~and ms (m ...)))
     (replace-context #'ms
                      #'(begin
                          (require (submod m untyped) ...)
                          (provide (all-from-out (submod m untyped) ...))))]))

(define-typed/untyped-identifier require/provide-typed/untyped
  require/provide-typed/untyped-typed
  require/provide-typed/untyped-untyped)

#|
(module mt typed/racket
  (define-syntax-rule (require/provide-typed/untyped m)
    (require m))
  (provide require/provide-typed/untyped))
(require 'mt)
|#

;; define-typed/untyped-modules
(begin
  (define-syntax (define-typed/untyped-modules stx)
    (syntax-parse stx
      [(def-t/u-mod (~optional (~and no-test #:no-test))
          (~optional (~and untyped-first #:untyped-first)) . body)
       (define (ds sym) (datum->syntax #'def-t/u-mod sym #'def-t/u-mod))
       (define/with-syntax module-typed
         #`(module #,(ds 'typed) #,(ds 'typed/racket)
             . body))
       (define/with-syntax module-untyped
         #`(module #,(ds 'untyped) #,(ds 'typed/racket/no-check)
             (#,(ds 'require) (#,(ds 'for-syntax) #,(ds 'racket/base)))
             . body))
       #`(begin
           #,(if (attribute untyped-first) #'module-untyped #'module-typed)
           #,(if (attribute untyped-first) #'module-typed #'module-untyped)
           #,@(if (attribute no-test)
                  #'()
                  #`((module #,(ds 'test) #,(ds 'typed/racket)
                       (#,(ds 'require) (#,(ds 'submod) #,(ds "..")
                                                        #,(ds 'typed)
                                                        #,(ds 'test)))
                       (#,(ds 'require) (#,(ds 'submod) #,(ds "..")
                                                        #,(ds 'untyped)
                                                        #,(ds 'test))))))
           (#,(ds 'require) '#,(ds 'typed))
           (#,(ds 'provide) (#,(ds 'all-from-out) '#,(ds 'typed))))]))
  
  #| ;; test: should work in no-check but not in typed:
  (define-typed/untyped-modules moo
    (: foo One)
    (define foo 2))
  |#)

;; if-typed
(define-syntax-rule (if-typed-typed t u) t)
(define-syntax-rule (if-typed-untyped t u) u)
(define-typed/untyped-identifier if-typed
  if-typed-typed
  if-typed-untyped)

;; when-typed and when-untyped
(define-syntax-rule (when-typed . t) (if-typed (begin . t) (begin)))
(define-syntax-rule (when-untyped . t) (if-typed (begin) (begin . t)))

;; typed/untyped-prefix
(begin
  (define-syntax-rule (typed/untyped-prefix [typed-prefix ...]
                                            [untyped-prefix ...]
                                            . rest)
    (if-typed (typed-prefix ... . rest)
              (untyped-prefix ... . rest)))
  #|
  ;; test: should work in no-check but not in typed:
  (typed/untyped-prefix
   [module moo2 typed/racket]
   [module moo2 typed/racket/no-check]
   (: foo One)
   (define foo 2))
  |#)

;; define-modules
(begin
  ;; define-modules
  (define-syntax define-modules
    (syntax-rules (no-submodule)
      [(_ ([no-submodule] [name lang] ...) . body)
       (begin (begin . body)
              (module name lang . body) ...)]
      [(_ ([name lang] ...) . body)
       (begin (module name lang . body) ...)]))
  
  #|
  ;; TODO: tests: test with a macro and check that we can use it in untyped.
  ;; TODO: tests: test with two mini-languages with different semantics for some
  ;; function.
  (define-modules ([foo typed/racket] [foo-untyped typed/racket/no-check])
    (provide x)
    (: x (→ Syntax Syntax))
    (define (x s) s))
  
  (module test racket
    (require (submod ".." foo-untyped))
    (x #'a))
  |#)