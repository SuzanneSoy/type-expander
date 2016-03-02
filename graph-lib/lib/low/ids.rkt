#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:untyped-first
  (provide !temp
           (rename-out [!temp &])
           format-ids
           hyphen-ids
           format-temp-ids
           #|!temp|#
           define-temp-ids)
  
  (require "typed-untyped.rkt")
  (require-typed/untyped "sequence.rkt"
                         "aliases.rkt")
  (begin-for-syntax (require "typed-untyped.rkt")
                    (require-typed/untyped "aliases.rkt"))
  
  (module m-!temp racket
    (provide !temp)
    
    (require syntax/parse
             syntax/parse/experimental/template)
    
    (define-template-metafunction (!temp stx)
      (syntax-parse stx
        [(_ id:id)
         #:with (temp) (generate-temporaries #'(id))
         #'temp]
        #|[(_ . id:id)
       #:with (temp) (generate-temporaries #'(id))
       #'temp]
      [(_ id:id ...)
       (generate-temporaries #'(id ...))]|#)))
  (require 'm-!temp)
  
  (require/typed racket/syntax
                 [format-id (→ Syntax String (U String Identifier) *
                               Identifier)])
  (require (only-in racket/syntax define/with-syntax)
           (only-in syntax/stx stx-map)
           (for-syntax racket/base
                       racket/syntax
                       syntax/parse
                       syntax/parse/experimental/template))
  ;(require racket/sequence) ;; in-syntax
  
  (define-type S-Id-List
    (U String
       Identifier
       (Listof String)
       (Listof Identifier)
       (Syntaxof (Listof Identifier))))
  
  ; TODO: format-ids doesn't accept arbitrary values. Should we change it?
  ; 
  (: format-ids (→ (U Syntax (→ (U String Identifier) * Syntax))
                   String
                   S-Id-List *
                   (Listof Identifier)))
  (define (format-ids lex-ctx format . vs)
    (let* ([seqs
            (map (λ ([v : S-Id-List])
                   (cond
                     [(string? v) (in-cycle (in-value v))]
                     [(identifier? v) (in-cycle (in-value v))]
                     [(list? v) (in-list v)]
                     [else (in-list (syntax->list v))]))
                 vs)]
           [justconstants (andmap (λ (x) (or (string? x) (identifier? x))) vs)]
           [seqlst (apply sequence-list seqs)])
      (for/list : (Listof Identifier)
        ([items seqlst]
         [bound-length (if justconstants
                           (in-value 'yes)
                           (in-cycle (in-value 'no)))])
        
        (apply format-id
               (if (procedure? lex-ctx) (apply lex-ctx items) lex-ctx)
               format
               items))))
  
  (: hyphen-ids (→ (U Syntax (→ (U String Identifier) * Syntax))
                   S-Id-List *
                   (Listof Identifier)))
  
  (define (hyphen-ids lex-ctx . vs)
    (apply format-ids
           lex-ctx
           (string-join (map (λ _ "~a") vs) "-")
           vs))
  
  (: format-temp-ids (→ String
                        S-Id-List *
                        (Listof Identifier)))
  
  (define (format-temp-ids format . vs)
    ;; Introduce the binding in a fresh scope.
    (apply format-ids (λ _ ((make-syntax-introducer) #'())) format vs))
  
  ;; Also in ==== syntax.rkt ====, once we split into multiple files, require it
  (begin-for-syntax
    (define (syntax-cons-property stx key v)
      (let ([orig (syntax-property stx key)])
        (syntax-property stx key (cons v (or orig '()))))))
  
  ;; Also in ==== syntax.rkt ====, once we split into multiple files, require it
  (begin-for-syntax
    (define (identifier-length id) (string-length (symbol->string
                                                   (syntax-e id)))))
  
  (begin-for-syntax
    (define-syntax-class dotted
      (pattern id:id
               #:attr make-dotted
               (λ (x) x)
               #:attr wrap
               (λ (x f) (f x #t)))
      (pattern (nested:dotted (~literal ...));(~and dots (~literal ...)) ...+)
               #:with id #'nested.id
               #:attr make-dotted
               (λ (x) #`(#,((attribute nested.make-dotted) x) (... ...)));dots …
               #:attr wrap
               (λ (x f) (f ((attribute nested.wrap) x f) #f))))
    
    (define-syntax-class simple-format
      (pattern format
               #:when (string? (syntax-e #'format))
               #:when (regexp-match #rx"^[^~]*~a[^~]*$" (syntax-e #'format))
               #:attr pos (regexp-match-positions #rx"^([^~]*)~a([^~]*)$"
                                                  (syntax-e #'format))
               #:attr left-start 1
               #:attr left-end (+ 1 (cdr (cadr (attribute pos))))
               #:attr left-len (cdr (cadr (attribute pos)))
               
               #:attr right-start (+ 1 (car (caddr (attribute pos))))
               #:attr right-end (+ 1 (cdr (caddr (attribute pos))))
               #:attr right-len (- (attribute right-end)
                                   (attribute right-start)))))
  
  (define-syntax (define-temp-ids stx)
    (syntax-parse stx
      #|
      ;; TODO : factor this with the next case.
      [(_ format ((base:id (~literal ...)) (~literal ...)))
       #:when (string? (syntax-e #'format))
       (with-syntax ([pat (format-id #'base (syntax-e #'format) #'base)])
         #'(define/with-syntax ((pat (... ...)) (... ...))
             (stx-map (curry format-temp-ids format)
                      #'((base (... ...)) (... ...)))))]
|#
      
      ;; New features (arrows and #:first) special-cased for now
      ;; TODO: make these features more general.
      [(_ format:simple-format base:dotted #:first-base first-base)
       #:with first (format-id #'first-base (syntax-e #'format) #'first-base)
       (let ([first-base-len (identifier-length #'first-base)])
         (syntax-cons-property #'(define-temp-ids format base #:first first)
                               'sub-range-binders
                               (list
                                (if (> (attribute format.left-len) 0)
                                    (vector (syntax-local-introduce #'first)
                                            0
                                            (attribute format.left-len)
                                            
                                            (syntax-local-introduce #'format)
                                            (attribute format.left-start)
                                            (attribute format.left-len))
                                    '())
                                (vector (syntax-local-introduce #'first)
                                        (attribute format.left-len)
                                        first-base-len
                                        
                                        (syntax-local-introduce #'first-base)
                                        0
                                        first-base-len)
                                (if (> (attribute format.right-len) 0)
                                    (vector (syntax-local-introduce #'first)
                                            (+ (attribute format.left-len)
                                               first-base-len)
                                            (attribute format.right-len)
                                            
                                            (syntax-local-introduce #'format)
                                            (attribute format.right-start)
                                            (attribute format.right-len))
                                    '()))))]
      
      [(_ format:simple-format
          base:dotted
          (~optional (~seq #:first first)))
       (let* ([base-len (string-length (symbol->string (syntax-e #'base.id)))])
         (define/with-syntax pat
           (format-id #'base.id (syntax-e #'format) #'base.id))
         (define/with-syntax pat-dotted ((attribute base.make-dotted) #'pat))
         
         (define/with-syntax format-temp-ids*
           ((attribute base.wrap) #'(compose car
                                             (curry format-temp-ids format)
                                             generate-temporary)
                                  (λ (x deepest?)
                                    (if deepest?
                                        x
                                        #`(curry stx-map #,x)))))
         
         (syntax-cons-property
          (template (begin (define/with-syntax pat-dotted
                             (format-temp-ids* #'base))
                           (?? (?@ (define/with-syntax (first . _)
                                     #'pat-dotted)))))
          'sub-range-binders
          (list (if (> (attribute format.left-len) 0)
                    (vector (syntax-local-introduce #'pat)
                            0
                            (attribute format.left-len)
                            
                            (syntax-local-introduce #'format)
                            (attribute format.left-start)
                            (attribute format.left-len))
                    '())
                (vector (syntax-local-introduce #'pat)
                        (attribute format.left-len)
                        base-len
                        
                        (syntax-local-get-shadower #'base.id)
                        0
                        base-len)
                (if (> (attribute format.right-len) 0)
                    (vector (syntax-local-introduce #'pat)
                            (+ (attribute format.left-len) base-len)
                            (attribute format.right-len)
                            
                            (syntax-local-introduce #'format)
                            (attribute format.right-start)
                            (attribute format.right-len))
                    '()))))]
      [(_ format base:dotted)
       #:when (string? (syntax-e #'format))
       #:when (regexp-match #rx"^[^~]*$" (syntax-e #'format))
       (define/with-syntax pat (format-id #'base (syntax-e #'format)))
       (define/with-syntax pat-dotted ((attribute base.make-dotted) #'pat))
       (define/with-syntax format-temp-ids*
         ((attribute base.wrap) #'(λ (x)
                                    (car (format-temp-ids
                                          (string-append format "~a")
                                          "")))
                                (λ (x deepest?)
                                  (if deepest?
                                      x
                                      #`(curry stx-map #,x)))))
       (syntax-cons-property
        #'(define/with-syntax pat-dotted
            (format-temp-ids* #'base))
        'sub-range-binders
        (list (vector (syntax-local-introduce #'pat)
                      0
                      (string-length (syntax-e #'format))
                      
                      (syntax-local-introduce #'format)
                      1
                      (string-length (syntax-e #'format)))))]
      [(_ name:id format:expr . vs)
       #`(define/with-syntax name (format-temp-ids format . vs))]))
  
  (module+ test
    (require-typed/untyped "typed-rackunit.rkt")
    (require ;(submod "..")
      (for-syntax racket/syntax
                  (submod ".." ".." untyped)))
    
    (check-equal?: (format-ids #'a "~a-~a" #'() #'())
                   '())
    
    (check-equal?: (map syntax->datum
                        (format-ids #'a "~a-~a" #'(x1 x2 x3) #'(a b c)))
                   '(x1-a x2-b x3-c))
    
    ;; Since the presence of "Syntax" in the parameters list makes format-ids
    ;; require a chaperone contract instead of a flat contract, we can't run the
    ;; two tests below directly, we would need to require the untyped version of
    ;; this file, which causes a cycle in loading.
    
    (define-syntax (test1 stx)
      (syntax-case stx ()
        [(_ (let1 d1) x y)
         (begin
           (define/with-syntax (foo-x foo-y)
             (format-ids (λ (xy)
                           (if (string=? (symbol->string (syntax->datum xy))
                                         "b")
                               stx
                               #'()))
                         "foo-~a"
                         #'(x y)))
           #'(let1 d1 (let ((foo-b 2) (foo-c 'b)) (cons foo-x foo-y))))]))
    
    (check-equal?: (test1 (let ((foo-b 1) (foo-c 'a))) b c)
                   '(1 . b))
    
    (define-syntax (fubar stx)
      (define/with-syntax (v1 ...) #'(1 2 3))
      (define/with-syntax (v2 ...) #'('a 'b 'c))
      ;; the resulting ab and ab should be distinct identifiers:
      (define/with-syntax (id1 ...) (format-temp-ids "~a" #'(ab cd ab)))
      (define/with-syntax (id2 ...) (format-temp-ids "~a" #'(ab cd ab)))
      #'(let ([id1 v1] ...)
          (let ([id2 v2] ...)
            (list (cons id1 id2) ...))))
    
    (check-equal?: (fubar) '((1 . a) (2 . b) (3 . c)))))