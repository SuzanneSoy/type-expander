#lang typed/racket

(require (for-syntax racket/syntax))

(define-syntax-rule (provide-id id)
  (begin
    (define-syntax (id stx)
      (raise-syntax-error 'id
                          (format "Type expander form “~a” cannot be used as an expression"
                                  'id)
                          stx))
    (provide id)))

(define-syntax-rule (provide-ids id ...) (begin (provide-id id) ...))

(provide-ids Let Letrec Λ ...* No-Expand)

;; Define a mutable implementation for new-:, circumvent the fact that
;; typed/racket wraps macros with a contract.
;;
;; Technique from:
;;
;; https://github.com/racket/typed-racket/issues/329#issuecomment-205060192

(define-syntax (provide-mutable-id stx)
  (syntax-case stx ()
    [(_ short-id)
     (with-syntax ([id (format-id #'short-id "new-~a" #'short-id)]
                   [id-set-impl (format-id #'short-id
                                           "set-~a-impl!"
                                           #'short-id)])
       #'(begin
           (provide id id-set-impl)
           
           (define-for-syntax (id-impl-orig stx)
             (raise-syntax-error ':
                                 (format "Implementation for ~a was not loaded!"
                                         'short-id)
                                 stx))
           
           (define-for-syntax id-impl (box id-impl-orig))

           (define-syntax (id stx)
             ((unbox id-impl) stx))
           
           (define-syntax-rule (id-set-impl impl)
             (begin-for-syntax
               (when (eq? (unbox id-impl) id-impl-orig)
                 (set-box! id-impl impl))))))]))

(define-syntax-rule (provide-mutable-ids id ...)
  (begin (provide-mutable-id id) ...))

(provide-mutable-ids :
                     ;; The class-related IDs need to also work as types.
                     ;field
                     ;super-new
                     )