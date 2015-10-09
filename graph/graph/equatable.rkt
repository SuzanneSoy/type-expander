#lang racket

(module untyped racket
  (provide (struct-out untyped-object))
  
  (define-struct untyped-object ()
    #:transparent
    ;#:property prop:procedure (λ (self . rest) (apply (untyped-object-proc self) rest))
    #:methods gen:custom-write
    [(define write-proc (λ (self port mode) (((vector-ref (struct->vector self) 1) 'write-proc) port mode)))]
    #:methods gen:equal+hash
    [(define equal-proc (λ (x y recursive-equal?) (((vector-ref (struct->vector x) 1) 'equal-proc) y recursive-equal?)))
     (define hash-proc (λ (x recursive-equal-hash-code?) (((vector-ref (struct->vector x) 1) 'hash-proc) recursive-equal-hash-code?)))
     (define hash2-proc (λ (x recursive-equal-secondary-hash-code?) (((vector-ref (struct->vector x) 1) 'hash2-proc) recursive-equal-secondary-hash-code?)))]))


(module typed typed/racket
  (require/typed (submod ".." untyped)
                 [#:struct untyped-object ()])
  
  (define-type Field-Present (Vector Any))

  (: field-present (→ Any Field-Present))
  (define (field-present x) (vector x))

  (: field-present-get-value (→ Field-Present Any))
  (define (field-present-get-value fp) (vector-ref fp 0))
  
  (struct (T) Equatable untyped-object
    ([f : (case→ [→ 'value T] ;; Sadly, we can't extend a case→ described by T, so we have to chain two calls to access any field.
                 ;; TODO: we could just directly accept the other parameters
                 [→ 'write-proc (→ Output-Port (U #t #f 0 1) Any)]
                 [→ 'equal-proc (→ (U Equatable Any) (→ Any Any Boolean) Boolean)]
                 [→ 'hash-proc (→ (→ Any Fixnum) Fixnum)]
                 [→ 'hash2-proc (→ (→ Any Fixnum) Fixnum)]
                 [→ 'reflect (→ (U Index Symbol) (U Field-Present #f))])])
    #:transparent)

  (: Equatable-value (∀ (T) (→ (Equatable T) T)))
  (define (Equatable-value e) ((Equatable-f e) 'value))
  
  (provide (struct-out Equatable)
           Equatable-value
           Field-Present
           field-present
           field-present-get-value))

(require 'typed)
(provide (all-from-out 'typed))
