#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  ;; raco pkg install alexis-util
  ;; or:
  ;; raco pkg install threading
  (require alexis/util/threading
           (for-syntax racket/syntax
                       syntax/parse))
  
  (define-syntax-rule (~>_ clause ... expr) (~> expr clause ...))
  (define-syntax (<~ stx)
    (syntax-parse stx
      [(_ expr clause ...)
       (define/with-syntax (r-clause ...)
         (reverse (syntax->list #'(clause ...))))
       #'(~> expr r-clause ...)]))
  
  (define-syntax-rule (<~_ clause ... expr) (<~ expr clause ...))
  
  (provide <~ <~_ ~>_ ~> ~>> _ (rename-out [_ ♦] [<~_ <~♦] [~>_ ~>♦])))