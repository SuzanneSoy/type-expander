#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules
  (provide sequence-length>=
           in-last?
           in-tails
           in-heads
           in-split
           in-split*
           *in-split
           Syntax-Listof
           my-in-syntax
           in-syntax
           sequence-cons
           sequence-null
           sequence-list)
  
  (require racket/sequence)
  
  ;; sequence-length>=
  (begin
    (: sequence-length>= (→ (Sequenceof Any) Index Boolean))
    (define (sequence-length>= s l)
      (let-values ([(more? next) (sequence-generate s)])
        (define (rec [remaining : Index]) : Boolean
          (if (= remaining 0)
              #t
              (and (more?)
                   (begin (next)
                          (rec (sub1 remaining))))))
        (rec l))))
  
  ;; in-last?
  ;; Returns a sequence of the same length as `s`. All values in the sequence
  ;; are #f, except for the last one which is 'last.
  (begin
    (: in-last? (→ (Sequenceof Any) (Sequenceof (U #f 'last))))
    (define (in-last? s)
      (if (sequence-length>= s 1)
          (sequence-append (sequence-map (λ _ #f) (sequence-tail s 1))
                           (in-value 'last))
          empty-sequence)))
  
  ;; in-heads and in-tails
  (begin
    (: in-tails (∀ (T) (→ (Listof T) (Listof (Pairof T (Listof T))))))
    (define (in-tails l)
      (if (null? l)
          '()
          (cons l (in-tails (cdr l)))))
    
    (module+ test
      (require typed/rackunit)
      (check-equal? (for/list : (Listof (Listof Number))
                      ([x : (Listof Number) (in-tails '(1 2 3 4 5))]) x)
                    '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)))
      (let ((l '(1 2 3 4 5)))
        (check-true (eq? (caddr (for/list : (Listof (Listof Number))
                                  ([x : (Listof Number) (in-tails l)]) x))
                         (cddr l)))))
    
    (: in-heads (∀ (T) (→ (Listof T) (Listof (Pairof T (Listof T))))))
    (define (in-heads l)
      (: my-append1 (→ (Listof T) T (Pairof T (Listof T))))
      (define (my-append1 x y)
        (if (null? x)
            (list y)
            (cons (car x) (my-append1 (cdr x) y))))
      
      (define (on-heads/private [acc-head : (Listof T)] [l : (Listof T)])
        : (Listof (Pairof T (Listof T)))
        (if (null? l)
            '()
            (let ([new-head (my-append1 acc-head (car l))])
              (cons new-head (on-heads/private new-head (cdr l))))))
      (on-heads/private '() l))
    
    (module+ test
      (require typed/rackunit)
      (check-equal? (for/list : (Listof (Listof Number))
                      ([x : (Listof Number) (in-heads '(1 2 3 4 5))]) x)
                    '((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)))))
  
  ;; in-split, in-split*, *in-split, *in-split*
  (begin
    ;; Can't write the type of in-split, because typed/racket doesn't allow
    ;; writing (Sequenceof A B), just (Sequenceof A).
    ;; in-parallel's type has access to the multi-valued version of Sequenceof,
    ;; though, so we let typed/racket propagate the inferred type.
    (define #:∀ (T) (in-split [l : (Listof T)])
      (in-parallel (sequence-append (in-value '()) (in-heads l))
                   (sequence-append (in-tails l) (in-value '()))))
    
    ;; Same as in-split, but without the empty tail.
    (define #:∀ (T) (in-split* [l : (Listof T)])
      (in-parallel (sequence-append (in-value '()) (in-heads l))
                   (sequence-append (in-tails l))))
    
    ;; Same as in-split, but without the empty head.
    (define #:∀ (T) (*in-split [l : (Listof T)])
      (in-parallel (in-heads l)
                   (sequence-append (sequence-tail (in-tails l) 1)
                                    (in-value '()))))
    
    (define #:∀ (T) (*in-split* [l : (Listof T)])
      (in-parallel (in-heads l)
                   (sequence-tail (in-tails l) 1))))
  
  ;; my-in-syntax and Syntax-Listof
  (begin
    ;; See also syntax-e, which does not flatten syntax pairs, and syntax->list,
    ;; which isn't correctly typed (won't take #'(a . (b c d e))).
    (define-type (Syntax-Listof T)
      (Rec R (Syntaxof (U Null
                          (Pairof T R)
                          (Listof T)))))
    
    ;; in-syntax is now provided by racket/sequence.
    (: my-in-syntax (∀ (T) (→ (Syntax-Listof T)
                              (Listof T))))
    (define (my-in-syntax stx)
      (let ((e (syntax-e stx)))
        (if (null? e)
            e
            (if (syntax? (cdr e))
                (cons (car e) (my-in-syntax (cdr e)))
                e))))
    
    (define (test-in-syntax)
      ; (ann `(,#'(a . b) ,#'(c . d))
      ;      (Listof (Syntaxof (U (Pairof (Syntaxof 'a) (Syntaxof 'b))
      ;                           (Pairof (Syntaxof 'c) (Syntaxof 'c))))))
      (my-in-syntax #'((a . b) (c . d)))
      ; (ann `(,#'a ,#'b ,#'c ,#'d ,#'e) (Listof (Syntaxof (U 'a 'b 'c 'd))))
      (my-in-syntax #'(a . (b c d e)))
      ; (ann '() (Listof (Syntaxof Nothing)))
      (my-in-syntax #'())))
  
  ;; combining sequences:
  ;; sequence-cons
  ;; sequence-null
  ;; sequence-list
  
  (begin
    (: sequence-cons (∀ (A B) (→ (Sequenceof A) (Sequenceof B)
                                 (Sequenceof (cons A B)))))
    (define (sequence-cons sa sb)
      (sequence-map (λ ([x : (List A B)]) (cons (car x) (cadr x)))
                    (in-values-sequence (in-parallel sa sb))))
    
    (: sequence-null (Sequenceof Null))
    (define sequence-null (in-cycle (in-value '())))
    
    ;; sequence-list should have the type:
    ;; (∀ (A ...) (→ (Sequenceof A) ... (Sequenceof (List A ...)))))
    ;; But the type system rejects the two definitions below.
    (: sequence-list (∀ (A) (→ (Sequenceof A) *
                               (Sequenceof (Listof A)))))
    (define (sequence-list . sequences)
      (if (null? sequences)
          sequence-null
          (sequence-cons (car sequences)
                         (apply sequence-list (cdr sequences)))))
    
    #|
    (: sequence-list (∀ (A ...) (→ (Sequenceof A) ...
                                   (Sequenceof (List A ...)))))
    (define (sequence-list . sequences)
      (if (null? sequences)
          sequence-null
          (sequence-cons (car sequences)
                         (apply sequence-list (cdr sequences)))))
    |#
    
    #|
    (: sequence-list (∀ (F R ...)
                        (case→ [→ (Sequenceof Null)]
                               [→ (Sequenceof F) (Sequenceof R) ...
                                  (Sequenceof (List F R ...))])))
    (define sequence-list
      (case-lambda
        [()
         sequence-null]
        [(sequence . sequences)
         (sequence-cons sequence (apply sequence-list sequences))]))
    |#))