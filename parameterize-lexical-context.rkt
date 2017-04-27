#lang racket

(require (for-template racket/base)
         syntax/parse
         syntax/id-table
         (for-syntax syntax/parse
                     racket/syntax
                     syntax/parse/experimental/template)
         debug-scopes)

(provide with-bindings
         with-rec-bindings
         tl-redirections
         start-tl-redirections
         f-start-tl-redirections
         binding-table-find-best
         binding-table-set!
         make-binding-table)

(struct binding-table-struct (val))

(define/contract tl-redirections
  (parameter/c (or/c binding-table-struct? #f))
  (make-parameter #f))

(define (make-binding-table)
  (-> binding-table-struct?)
  (binding-table-struct (make-hasheq)))

(define/contract (binding-table-set! table id value)
  (-> binding-table-struct? identifier? any/c void?)
  (let ([group (hash-ref! (binding-table-struct-val table)
                          (syntax-e id)
                          (make-bound-id-table))])
    (when (dict-has-key? group id)
      (raise-syntax-error
       'type-expander
       "Attempted to re-bind the same identifier with the same scopes"
       id))
    (bound-id-table-set! group id value)))

(define (binding-table-find-best table id fallback)
  (-> binding-table-struct? identifier? (or/c procedure? any/c) void?)
  (define (scopes-of i)
    (list->set (map (λ (v) (vector-ref v 0))
                    (hash-ref (syntax-debug-info i) 'context))))
  (define scopes-of-id (scopes-of id))
  (let* ([group (hash-ref (binding-table-struct-val table)
                          (syntax-e id)
                          (λ () (make-bound-id-table)))]
         [candidates (filter (λ (other)
                               (subset? (car other) scopes-of-id))
                             (bound-id-table-map group
                                                 (λ (a b)
                                                   (list (scopes-of a) a b))))])
    (if (= 0 (length candidates))
        (if (procedure? fallback)
            (fallback)
            fallback)
        (let* ([best-candidate (argmax (λ (c) (set-count (car c)))
                                       candidates)])
          (for ([c candidates])
            (unless (subset? (car c) (car best-candidate))
              (raise-syntax-error 'type-expander
                                  (format "Ambiguous bindings: ~a"
                                          (map (λ (c) (list (cadr c) (car c)))
                                               candidates)))))
          (caddr best-candidate)))))

(define-syntax-rule (start-tl-redirections . rest)
  (parameterize ([tl-redirections (or (tl-redirections)
                                      (make-binding-table))])
    . rest))

(define-syntax-rule (f-start-tl-redirections f)
  (λ l (start-tl-redirections (apply f l))))


(define-syntax with-bindings
  (syntax-parser
    [(_ [{~or v1:id (v* {~and ooo {~literal ...}})} e/es] x code ...+)
     #:with vs (if (attribute ooo) #'(v* ooo) #'(v1))
     #:with es (if (attribute ooo) #'e/es #'(list e/es))
     (template
      (let ()
        (define ctx (make-syntax-introducer))
        (invariant-assertion (λ (ll) (and (list? ll)
                                          (andmap identifier? ll)))
                             (syntax->list #'vs))
        (for ([binding (in-syntax #'vs)]
              [value es])
          (binding-table-set! (tl-redirections) (ctx binding) value))
        (with-syntax ([(vs x)
                       (ctx #'(vs x))])
          code ...)))]))

(define-syntax with-rec-bindings
  (syntax-parser
    [(_ [{~or v1:id (v* {~and ooo {~literal ...}})} func e/es] x code ...+)
     #:with vs (if (attribute ooo) #'(v* ooo) #'(v1))
     #:with es (if (attribute ooo) #'(e/es ooo) #'(e/es))
     (template
      (let ()
        (define ctx (make-syntax-introducer))
        (define ctx2 (make-syntax-introducer #t))
        (invariant-assertion (λ (ll) (and (list? ll)
                                          (andmap identifier? ll)))
                             (syntax->list #'vs))
        (for ([binding (in-syntax #'vs)]
              [stx-value (in-syntax #'es)])
          (let ([vvv (func (ctx stx-value))])
            (binding-table-set! (tl-redirections)
                                (ctx binding)
                                vvv)))
        (with-syntax ([(vs x)
                       (ctx2 (ctx #'(vs x)))])
          code ...)))]))

(provide trampoline-eval)
(define trampoline-result (make-parameter #f))
(define (trampoline-eval code)
  (define result 'not-yet-result)
  (parameterize ([trampoline-result (λ (v) (set! result v))])
    (local-expand (syntax-local-introduce
                   #`(let-syntax ([tr ((trampoline-result) #,code)])
                       (void)))
                  'expression
                  '()))
  result)


(module+ test
  (require rackunit)
  (check-equal? (let ()
                  (define tbl (make-binding-table))
                  (define id #'id)
                  (binding-table-set! tbl id 123)
                  (define ctx (make-syntax-introducer))
                  (binding-table-set! tbl (ctx id) 456)
                  (define ctx2 (make-syntax-introducer))
                  (list (binding-table-find-best tbl id #f)
                        (binding-table-find-best tbl (ctx id) #f)
                        (binding-table-find-best tbl (ctx2 id) #f)
                        (binding-table-find-best tbl (ctx2 (ctx id)) #f)
                        (binding-table-find-best tbl (ctx (ctx2 id)) #f)))
                '(123 456 123 456 456)))