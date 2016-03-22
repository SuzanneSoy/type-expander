#lang typed/racket

(module test typed/racket
  (require (for-syntax (submod "rewrite-type.lp2.rkt" test-syntax)
                       syntax/strip-context))
  
  (define-syntax (insert-tests stx)
    (replace-context stx tests))
  
  (require (for-syntax "rewrite-type.lp2.rkt")
           typed/rackunit
           "../type-expander/multi-id.lp2.rkt"
           "../type-expander/type-expander.lp2.rkt")
  
  (insert-tests)
  
  ;; make-fold
  (define-syntax (make-fold stx)
    (syntax-case stx ()
      [(_ name type acc-type [from to pred? fun] ...)
       #`(begin
           (: name (→ type
                      acc-type
                      (Pairof #,(replace-in-type #'type #'([from to] ...))
                              acc-type)))
           (define (name [val : type] [acc : acc-type])
             (let-values ([([res : #,(replace-in-type #'type
                                                      #'([from to] ...))]
                            [res-acc : acc-type])
                           (#,(fold-instance #'type
                                             #'acc-type
                                             #'([from to pred? fun] ...))
                            val
                            acc)])
               (cons res res-acc))))]))
  
  ;; fold-instance
  (begin
    (make-fold test-fold-1
               (List String Number (List String String Symbol String))
               Number
               [String Number string? (λ ([x : String] [acc : Number])
                                        (values (string-length x)
                                                (+ acc (string-length x))))])
    
    (check-equal? (test-fold-1 '("a" 7 ("bb" "cccc" x "dddddddd")) 0)
                  '((1 7 (2 4 x 8)) . 15)))
  
  (begin
    (make-fold test-fold-list
               (List String Number (Pairof String String) Symbol)
               Number
               [String Number string? (λ ([x : String] [acc : Number])
                                        (values (string-length x)
                                                (+ acc (string-length x))))])
    
    (check-equal? (test-fold-list '("a" 9 ("bb" . "cccc") x) 0)
                  '((1 9 (2 . 4) x) . 7)))
  
  (begin
    (make-fold test-fold-pairof
               (Pairof String (Pairof Number String))
               Number
               [String Number string? (λ ([x : String] [acc : Number])
                                        (values (string-length x)
                                                (+ acc (string-length x))))])
    
    (check-equal? (test-fold-pairof '("a" 7 . "bb") 0)
                  '((1 7 . 2) . 3)))
  
  (begin
    (make-fold test-fold-listof
               (List String Number (Listof String) Symbol String)
               Number
               [String Number string? (λ ([x : String] [acc : Number])
                                        (values (string-length x)
                                                (+ acc (string-length x))))])
    
    (check-equal? (test-fold-listof
                   '("a" 7 ("bb" "cccc" "dddddddd") x "eeeeeeeeeeeeeeee")
                   0)
                  '((1 7 (2 4 8) x 16) . 31)))
  
  (begin
    (make-fold test-fold-vector
               (Vector String Number (Vectorof String) Symbol String)
               Number
               [String Number string? (λ ([x : String] [acc : Number])
                                        (values (string-length x)
                                                (+ acc (string-length x))))])
    
    (check-equal? (test-fold-vector
                   '#("a" 7 #("bb" "cccc" "dddddddd") x "eeeeeeeeeeeeeeee")
                   0)
                  '(#(1 7 #(2 4 8) x 16) . 31)))
  
  (begin
    (make-fold test-fold-vectorof
               (Vectorof (U (List 'tag1 String String) (List 'tag2 Number)))
               Number
               [String Number string? (λ ([x : String] [acc : Number])
                                        (values (string-length x)
                                                (+ acc (string-length x))))])
    
    (check-equal? (test-fold-vectorof
                   '#((tag1 "a" "bb") (tag2 7) (tag1 "cccc" "dddddddd"))
                   0)
                  '(#((tag1 1 2) (tag2 7) (tag1 4 8)) . 15)))
  
  
  (begin
    (make-fold test-fold-big
               (List (Pairof (U (List 'tag1 (List (Vector Symbol)
                                                  Number
                                                  (Listof String)))
                                (List 'tag2 (List (Vector Symbol)
                                                  Number
                                                  (Listof String))))
                             String))
               Number
               [String Number string? (λ ([x : String] [acc : Number])
                                        (values (string-length x)
                                                (+ acc (string-length x))))])
    
    (check-equal?
     (test-fold-big '(((tag2 (#(sym) 7 ("a" "bb" "cccc"))) . "dddddddd")) 0)
     '((((tag2 (#(sym) 7 (1 2 4))) . 8)) . 15))))