#lang racket

(require syntax/parse/experimental/template
         syntax/parse
         (for-syntax racket/syntax))

(provide meta-struct?
         (struct-out meta-struct-info)
         get-meta-struct-info
         ;; More provided by `shorthand` in the code below
         meta-struct-subtype?
         struct-predicate
         struct-constructor
         struct-accessor)

(module info racket
  (require racket/struct-info)
  
  (provide meta-struct?
           (struct-out meta-struct-info)
           get-meta-struct-info)
  
  (define (meta-struct? s)
    (and (identifier? s)
         (let ([v (syntax-local-value s (λ _ #f))])
           (and v (struct-info? v)))))
  
  (struct meta-struct-info
    (type-descriptor
     constructor
     predicate
     accessors
     mutators
     super-type)
    #:transparent)
  
  (define (get-meta-struct-info s #:srcloc [srcloc #f])
    (if (meta-struct? s)
        (apply meta-struct-info (extract-struct-info (syntax-local-value s)))
        (raise-syntax-error 'get-struct-info
                            "not a structure definition"
                            (or srcloc s)
                            s))))

(require 'info
         (for-syntax 'info))

(define-syntax (shorthand stx)
  (syntax-case stx ()
    [(_ base)
     (with-syntax ([name (format-id #'base "meta-struct-~a" #'base)]
                   [accessor (format-id #'base "meta-struct-info-~a" #'base)]
                   [tmpl (format-id #'base "tmpl-struct-~a" #'base)])
       #'(begin
           (provide name tmpl)
           (define-template-metafunction (tmpl stx)
             (syntax-parse stx
               [(_ s (~optional (~seq #:srcloc srcloc)))
                (accessor
                 (get-meta-struct-info #'s #:srcloc (attribute srcloc)))]))
           (define (name s #:srcloc [srcloc #f])
             (accessor
              (get-meta-struct-info s #:srcloc srcloc)))))]))

(shorthand type-descriptor)
(shorthand constructor)
(shorthand predicate)
(shorthand accessors)
(shorthand mutators)
(shorthand super-type)

(define-syntax (struct-predicate stx)
  (syntax-case stx ()
    [(_ s) (meta-struct-info-predicate (get-meta-struct-info #'s))]))
(define-syntax (struct-constructor stx)
  (syntax-case stx ()
    [(_ s) (meta-struct-info-constructor (get-meta-struct-info #'s))]))
(define-syntax (struct-accessor stx)
  (syntax-case stx ()
    [(_ s i) (list-ref (meta-struct-info-accessors (get-meta-struct-info #'s))
                       (syntax-e #'i))]))

(define (meta-struct-subtype? sub super)
  (or (equal? (meta-struct-type-descriptor sub)
              (meta-struct-type-descriptor super))
      (let ((up (meta-struct-super-type sub)))
        (and (meta-struct? up)
             (meta-struct-subtype? up super)))))
