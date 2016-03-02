#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide cond-let)
  
  (require (for-syntax syntax/parse
                       (submod "aliases.rkt" untyped)))
  
  (define-syntax (cond-let stx)
    (syntax-parse stx
      [(_)
       #'(typecheck-fail #,stx)]
      [(_ #:let bindings:expr clause …)
       #'(let bindings (cond-let clause …))]
      [(_ [condition:expr (~seq #:else-let binding …) … . body] clause …)
       #'(if condition
             (begin . body)
             (let (binding … …)
               (cond-let clause …)))])))