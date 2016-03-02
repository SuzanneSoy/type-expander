#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules
  (provide stx-list
           stx-e
           stx-pair
           
           syntax-cons-property
           stx-map-nested
           identifier-length
           identifier->string
           (rename-out [identifier->string identifier→string])
           ;stx-map-nested
           
           stx-car
           stx-cdr
           stx-null?
           stx-pair?
           
           stx-cons
           
           Stx-List?
           Syntax-Pairs-of
           
           stx-drop-last
           
           stx-foldl
           
           stx-assoc
           cdr-stx-assoc
           
           check-duplicate-identifiers
           
           nameof)
  
  (require "typed-untyped.rkt")
  (require-typed/untyped "sequence.rkt")
  
  ;; match-expanders:
  ;;   stx-list
  ;;   stx-e
  ;;   stx-pair
  (begin
    (define-match-expander stx-list
      (lambda (stx)
        (syntax-case stx ()
          [(_ pat ...)
           #'(? syntax?
                (app syntax->list (list pat ...)))])))
    
    (module+ test
      (require typed/rackunit)
      (check-equal? (match #'(1 2 3)
                      [(stx-list a b c) (list (syntax-e c)
                                              (syntax-e b)
                                              (syntax-e a))])
                    '(3 2 1))
      
      (check-equal? (match #'(1 2 3)
                      [(stx-list a ...) (map (inst syntax-e Positive-Byte) a)])
                    '(1 2 3))
      
      #;(check-equal? (match #`(1 . (2 3))
                        [(stx-list a b c) (list (syntax-e c)
                                                (syntax-e b)
                                                (syntax-e a))])
                      '(3 2 1)))
    
    ;; stx-e
    (define-match-expander stx-e
      (lambda (stx)
        (syntax-case stx ()
          [(_ pat)
           #'(? syntax?
                (app syntax-e pat))])))
    
    (module+ test
      (require typed/rackunit)
      (check-equal? (match #'x [(stx-e s) s]) 'x)
      (check-equal? (match #'(x . y) [(stx-e (cons a b)) (cons (syntax-e b)
                                                               (syntax-e a))])
                    '(y . x)))
    
    (define-match-expander stx-pair
      (lambda (stx)
        (syntax-case stx ()
          [(_ pat-car pat-cdr)
           #'(? syntax?
                (app syntax-e (cons pat-car pat-cdr)))])))
    
    (module+ test
      (require typed/rackunit)
      (check-equal? (match #'(x . y) [(stx-pair a b) (cons (syntax-e b)
                                                           (syntax-e a))])
                    '(y . x))
      (check-equal? (match #'(x y z) [(stx-pair a b) (cons (map syntax->datum b)
                                                           (syntax->datum a))])
                    '((y z) . x))))
  
  ;; utilities:
  ;;   syntax-cons-property
  ;;   identifier-length
  ;;   identifier->string
  ;;   stx-map-nested
  (begin
    (: syntax-cons-property (∀ (A) (→ (Syntaxof A) Symbol Any (Syntaxof A))))
    (define (syntax-cons-property stx key v)
      (let ([orig (syntax-property stx key)])
        (syntax-property stx key (cons v (or orig '())))))
    
    (: identifier-length (→ Identifier Index))
    (define (identifier-length id) (string-length (identifier->string id)))
    
    (: identifier->string (→ Identifier String))
    (define (identifier->string id) (symbol->string (syntax-e id)))
    
    (: stx-map-nested (∀ (A B) (→ (→ A B)
                                  (Syntaxof (Listof (Syntaxof (Listof A))))
                                  (Listof (Listof B)))))
    (define (stx-map-nested f stx)
      (map (λ ([x : (Syntaxof (Listof A))])
             (map f (syntax-e x)))
           (syntax-e stx))))
  
  ;; accessors:
  ;;   stx-car
  ;;   stx-cdr
  ;;   stx-null?
  ;;   stx-pair?
  (begin
    #|
    (require/typed syntax/stx
                   [stx-car (∀ (A B) (→ (Syntaxof (Pairof A B)) A))]
                   [stx-cdr (∀ (A B) (→ (Syntaxof (Pairof A B)) B))])
    |#
    
    (: stx-car (∀ (A B)
                  (case→ (→ (Syntaxof (Pairof A B)) A)
                         ;; TODO: Not typesafe!
                         (→ (U (Syntaxof (Listof A)) (Listof A)) A))))
    (define (stx-car p) (car (if (syntax? p) (syntax-e p) p)))
    
    (: stx-cdr (∀ (A B)
                  (case→ (→ (Syntaxof (Pairof A B))
                            B)
                         ;; TODO: Not typesafe!
                         (→ (U (Syntaxof (Listof A)) (Listof A))
                            (Listof A)))))
    (define (stx-cdr p) (cdr (if (syntax? p) (syntax-e p) p)))
    
    (: stx-null? (→ Any Boolean : (U (Syntaxof Null) Null)))
    (define (stx-null? v)
      ((make-predicate (U (Syntaxof Null) Null)) v))
    
    (: stx-pair? (→ Any Boolean : (U (Pairof Any Any)
                                     (Syntaxof (Pairof Any Any)))))
    (define (stx-pair? v)
      ((make-predicate (U (Pairof Any Any)
                          (Syntaxof (Pairof Any Any))))
       v)))
  
  ;; constructors:
  ;;   stx-cons
  (begin
    (module m-stx-cons-untyped racket
      (provide stx-cons list->stx list*->stx)
      
      (define (stx-cons a b) #`(#,a . #,b))
      (define (list->stx l) #`#,l)
      (define (list*->stx l*) #`#,l*))
    
    (if-typed
     (module m-stx-cons-typed typed/racket
       (provide stx-cons list->stx list*->stx)
       (require (only-in typed/racket/unsafe unsafe-require/typed))
       (unsafe-require/typed
        (submod ".." m-stx-cons-untyped)
        [stx-cons (∀ (A B)
                     (→ (Syntaxof A)
                        (Syntaxof B)
                        (Syntaxof (Pairof (Syntaxof A) (Syntaxof B)))))]
        [list->stx (∀ (A)
                      (→ (Listof (Syntaxof A))
                         (Syntaxof (Listof (Syntaxof A)))))]
        [list*->stx (∀ (A B)
                       (→ (Rec R (U B (Pairof (Syntaxof A) R)))
                          (Syntaxof (Rec R (U B (Pairof (Syntaxof A) R))))))]))
     (module m-stx-cons-typed racket
       (provide stx-cons list->stx list*->stx)
       (require (submod ".." m-stx-cons-untyped))))
    
    (require 'm-stx-cons-typed)
    
    (module+ test
      (require ;(submod "..")
        typed/rackunit)
      
      (check-equal? (syntax->datum
                     (ann (stx-cons #'a #'(b c))
                          (Syntaxof (Pairof (Syntaxof 'a)
                                            (Syntaxof (List (Syntaxof 'b)
                                                            (Syntaxof 'c)))))))
                    '(a b c))
      
      (check-equal? (syntax->datum
                     (ann (stx-cons #'1 (ann #'2 (Syntaxof 2)))
                          (Syntaxof (Pairof (Syntaxof 1)
                                            (Syntaxof 2)))))
                    '(1 . 2))))
  
  ;; stx-drop-last
  (begin
    (: drop-last (∀ (A) (→ (Listof A) (Listof A))))
    (define (drop-last l)
      (if (and (pair? l) (pair? (cdr l)))
          (cons (car l) (drop-last (cdr l)))
          '()))
    
    (define-type (Stx-List? A)
      (U Null
         (Pairof A (Stx-List? A))
         (Syntaxof Null)
         (Syntaxof (Pairof A (Stx-List? A)))))
    
    (define-type (Syntax-Pairs-of A)
      (U (Syntaxof Null)
         (Syntaxof (Pairof A (Syntax-Pairs-of A)))))
    
    (module+ test
      (require-typed/untyped "typed-rackunit.rkt")
      
      (check-ann #'() (Stx-List? (Syntaxof Number)))
      (check-ann #'(1) (Stx-List? (Syntaxof Number)))
      (check-ann #'(1 2 3) (Stx-List? (Syntaxof Number)))
      (check-ann #'(1 2 . ()) (Stx-List? (Syntaxof Number)))
      (check-ann #'(1 . (2 . (3 . ()))) (Stx-List? (Syntaxof Number)))
      (check-ann #'(1 . (2 3 . ())) (Stx-List? (Syntaxof Number)))
      (check-ann #'(1 2 . (3 4 . (5))) (Stx-List? (Syntaxof Number))))
    
    (: stx->list (∀ (A) (→ (Stx-List? (Syntaxof A)) (Listof (Syntaxof A)))))
    (define (stx->list l)
      (cond [(null? l)
             '()]
            [(pair? l)
             (cons (car l) (stx->list (cdr l)))]
            [else
             (stx->list (syntax-e l))]))
    
    (: stx-drop-last
       (∀ (A) (→ (Stx-List? (Syntaxof A)) (Syntaxof (Listof (Syntaxof A))))))
    (define (stx-drop-last l)
      (list->stx (drop-last (stx->list l))))
    #|
      #;(cond [(null? l)
             #'()]
            [(pair? l)
             (cond [(null? (cdr l))
                    #'()]
                   [(pair? (cdr l))
                    ]
                   [else
             (let* ([res (stx-drop-last (cdr l))]
                    [e (syntax-e res)])
               (if (null? e)
                   (stx-cons (car l) #'())
                   (stx-cons (car l) res)))]
            [else
             (stx-drop-last (syntax-e l))])
      
      #;(if ((make-predicate (Syntaxof Any)) l)
          (stx-drop-last (syntax-e l))
          (if (null? l)
              #'()
              (stx-cons (car l)
                        (stx-drop-last (cdr l)))))))
      |#)
  
  ;; stx-foldl
  (begin
    (: stx-foldl
       (∀ (E F G Acc)
          (case→ (→ (→ E Acc Acc)
                    Acc
                    (U (Syntaxof (Listof E)) (Listof E))
                    Acc)
                 (→ (→ E F Acc Acc)
                    Acc
                    (U (Syntaxof (Listof E)) (Listof E))
                    (U (Syntaxof (Listof F)) (Listof F))
                    Acc)
                 (→ (→ E F G Acc Acc)
                    Acc
                    (U (Syntaxof (Listof E)) (Listof E))
                    (U (Syntaxof (Listof F)) (Listof F))
                    (U (Syntaxof (Listof G)) (Listof G))
                    Acc))))
    (define stx-foldl
      (case-lambda
        [(f acc l)
         (if (stx-null? l)
             acc
             (stx-foldl f (f (stx-car l) acc) (stx-cdr l)))]
        [(f acc l l2)
         (if (or (stx-null? l) (stx-null? l2))
             acc
             (stx-foldl f
                        (f (stx-car l) (stx-car l2) acc)
                        (stx-cdr l)
                        (stx-cdr l2)))]
        [(f acc l l2 l3)
         (if (or (stx-null? l) (stx-null? l2) (stx-null? l3))
             acc
             (stx-foldl f
                        (f (stx-car l) (stx-car l2) (stx-car l3) acc)
                        (stx-cdr l)
                        (stx-cdr l2)
                        (stx-cdr l3)))])))
  
  ;; stx-assoc
  ;; cdr-stx-assoc
  (begin
    (: stx-assoc (∀ (T) (case→
                         (→ Identifier
                            (U (Syntaxof (Listof (Syntaxof (Pairof Identifier
                                                                   T))))
                               (Listof (Syntaxof (Pairof Identifier T))))
                            (U (Syntaxof (Pairof Identifier T)) #f))
                         (→ Identifier
                            (Listof (Pairof Identifier T))
                            (U (Pairof Identifier T) #f)))))
    (define (stx-assoc id alist)
      (let* ([e-alist (if (syntax? alist)
                          (syntax->list alist)
                          alist)]
             [e-e-alist (cond
                          [(null? e-alist) '()]
                          [(syntax? (car e-alist))
                           (map (λ ([x : (Syntaxof (Pairof Identifier T))])
                                  (cons (stx-car x) x))
                                e-alist)]
                          [else
                           (map (λ ([x : (Pairof Identifier T)])
                                  (cons (car x) x))
                                e-alist)])]
             [result (assoc id e-e-alist free-identifier=?)])
        (if result (cdr result) #f)))
    
    (: cdr-stx-assoc
       (∀ (T) (case→ (→ Identifier
                        (U (Syntaxof (Listof (Syntaxof (Pairof Identifier T))))
                           (Listof (Syntaxof (Pairof Identifier T)))
                           (Listof (Pairof Identifier T)))
                        (U T #f)))))
    (define (cdr-stx-assoc id alist)
      (if (null? alist)
          #f
          ;; The typechecker is not precise enough, and the code below does not
          ;; work if we factorize it:
          ;; (if (and (list? alist) (syntax? (car alist))) … …)
          (if (list? alist)
              (if (syntax? (car alist))
                  (let ((res (stx-assoc id alist)))
                    (if res (stx-cdr res) #f))
                  (let ((res (stx-assoc id alist)))
                    (if res (cdr res) #f)))
              (let ((res (stx-assoc id alist)))
                (if res (stx-cdr res) #f))))))
  
  ;; check-duplicate-identifiers
  (begin
    (: check-duplicate-identifiers (→ (Syntaxof (Listof (Syntaxof Symbol)))
                                      Boolean))
    (define (check-duplicate-identifiers ids)
      (if (check-duplicate-identifier (my-in-syntax ids)) #t #f)))
  
  ;; nameof
  (begin
    ;; TODO: use the proper way to introduce arrows if possible.
    (define-syntax-rule (nameof x) (begin x 'x))
    
    (module+ test
      (require typed/rackunit)
      (let ((y 3))
        (check-equal? (nameof y) 'y))))
  
  #|
  (define (raise-multi-syntax-error name message exprs)
    (let ([e (exn:fail:syntax "message"
                              (current-continuation-marks)
                              (list #'aaa #'bbb))])
      ((error-display-handler) (exn-message e) e)))
  |#)