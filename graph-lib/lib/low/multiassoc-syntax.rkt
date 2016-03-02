#lang typed/racket
(require "../low2/typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide multiassoc-syntax
           cdr-assoc-syntax
           assoc-syntax)
  
  (require "../low2/typed-untyped.rkt")
  (require-typed/untyped "../low2/aliases.rkt"
                         "../low2/stx.rkt")
  
  ;; TODO: cdr-stx-assoc is already defined in lib/low.rkt
  
  (define-type (Stx-AList A)
    (Syntaxof (Listof (Syntaxof (Pairof Identifier A)))))
  
  (: multiassoc-syntax (∀ (A) (→ Identifier (Stx-AList A) (Listof A))))
  (define (multiassoc-syntax query alist)
    ((inst map A (Syntaxof (Pairof Identifier A)))
     stx-cdr
     (filter (λ ([xy : (Syntaxof (Pairof Identifier A))])
               (free-identifier=? query (stx-car xy)))
             (syntax->list alist))))
  
  (: cdr-assoc-syntax (∀ (A) (→ Identifier (Stx-AList A) A)))
  (define (cdr-assoc-syntax query alist)
    (stx-cdr (assert (assoc-syntax query alist))))
  
  (: assoc-syntax (∀ (A) (→ Identifier
                            (Stx-AList A)
                            (U False (Syntaxof (Pairof Identifier A))))))
  (define (assoc-syntax query alist)
    (findf (λ ([xy : (Syntaxof (Pairof Identifier A))])
             (free-identifier=? query (stx-car xy)))
           (syntax->list alist))))
