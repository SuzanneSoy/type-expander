#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide cars cdrs)
  
  #|
  ;; This does not work, in the end.
  (provide imap)
  (define-syntax (imap stx)
    (syntax-parse stx
      [(_ lst:expr var:id (~optional (~literal →)) . body)
       #'(let ()
           (define #:∀ (T) (inlined-map [l : (Listof T)])
             (if (null? l)
                 '()
                 (cons (let ([var (car l)]) . body)
                       (inlined-map (cdr l)))))
           (inlined-map lst))]))
  |#
  
  (: cars (∀ (A) (→ (Listof (Pairof A Any)) (Listof A))))
  (define (cars l) ((inst map A (Pairof A Any)) car l))
  
  (: cdrs (∀ (B) (→ (Listof (Pairof Any B)) (Listof B))))
  (define (cdrs l) ((inst map B (Pairof Any B)) cdr l)))