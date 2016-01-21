#lang typed/racket

(module dotlang racket
  (require typed/racket)
  
  (provide (except-out (all-from-out typed/racket) #%top)
           (rename-out [new-#%top #%top]))
  
  (require "graph4.lp2.rkt"
           "../lib/low-untyped.rkt"
           (for-syntax racket/string
                       syntax/parse
                       racket/syntax
                       "../lib/low-untyped.rkt"))
  
  (define-syntax/parse (dot x:id)
    (let* ([str (symbol->string (syntax-e #'x))]
           [components (regexp-match* #px"([^.…]|\\.\\.+)+|…" str)]
           [unescaped (map (λ (m) (regexp-replace* #px"\\.(\\.+)" m "\\1"))
                           components)]
           [identifiers (map (λ (u) (datum->syntax #'x (string->symbol u)))
                             unescaped)]
           [leading-dot? (regexp-match #px"^(\\.|…)" str)]
           [trailing-dot? (regexp-match #px"\\.$" str)])
      (define/with-syntax (id …) identifiers)
      (cond
        [leading-dot? #'(λget id …)]
        [trailing-dot? (raise-syntax-error 'dot "Found trailing dot" #'x)]
        [else #'(get id …)])))
  
  (define-syntax (new-#%top stx)
    (syntax-case stx ()
      [(_  . x)
       (if (regexp-match #rx"\\." (symbol->string (syntax-e #'x)))
           #`(dot x)
           #'(#%top . x))])))

(require 'dotlang)
(provide (all-from-out 'dotlang))

#;(module test (submod ".." dotlang)
    (require typed/rackunit)
    (let ((foo.bar 42))
      (check-equal? foo.bar 42))
    (check-equal? foo.bar '(dot "foo" "bar")))
