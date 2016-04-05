#lang typed/racket

(module test typed/racket
  (require (for-syntax (submod "structure.lp2.rkt" test-syntax)
                       syntax/strip-context))
  
  (define-syntax (insert-tests stx)
    (replace-context stx tests))
  
  (require "structure.lp2.rkt"
           phc-toolkit
           "../type-expander/type-expander.lp2.rkt"
           typed/rackunit)
  
  (insert-tests)
  
  ;; structure-get field
  (begin
    (check-equal?:
     (structure-get ((make-structure-constructor a b c d) 1 "b" 'val-c 4) c)
     : 'val-c
     'val-c))
  
  ;; match-expander
  (begin
    (let ([test-match 
           (λ ([val : Any])
             (match val
               [(structure a b c y) (list a b c y)]
               [(structure d
                           [a (? number?)]
                           [c (? symbol?) 'value-c]
                           [b bb (? string?)])
                (list a bb c d)]
               [else 'other]))])
      (check-equal?: (test-match
                      ((make-structure-constructor a b c d) 1
                                                            "b"
                                                            'value-c
                                                            4))
                     '(1 "b" value-c 4))
      (check-equal?: (test-match
                      ((make-structure-constructor a b c y) 1 2 3 4))
                     '(1 2 3 4))
      (check-equal?: (test-match 'bad) 'other)))
  
  ;; type-expander
  (begin
    (check-equal?
     (structure-get (ann ((make-structure-constructor a b c) 1 "b" #t)
                         (structure [a Number] [c Boolean] [b String]))
                    b)
     "b"))
  
  ;; structure
  (begin
    (let ()
      (define-structure empty-st)
      (define-structure stA [a Number])
      ;; BUG 137 (check-equal?: (empty-st) ((structure #:make-instance)))
      (check-not-equal?: (empty-st) (structure [a 1]))
      (check-not-equal?: (structure #:make-instance) (structure [a 1]))
      (check-not-equal?: (empty-st) (stA 1))
      (check-not-equal?: (structure #:make-instance) (stA 1))
      (void))
    
    ;; TODO: uncomment these tests:
    #;(let ()
        (define-structure st [a Number] [b String])
        (define-structure stA [a Number])
        (define-structure stABC [a Number] [b String] [c Number])
        (define st1 (st 1 "b"))
        (define st2 (st 2 "b"))
        (define sta (stA 1))
        (define st3 (stABC 1 "b" 3))
        
        (check-equal?-classes:
         [#:name st1
          st1
          (structure [a 1] [b "b"])
          (structure [a : Number 1] [b : String "b"])
          ((structure [a : Number] [b : String]) 1 "b")
          (structure [a : Any 1] [b : Any "b"])
          ((structure [a : Any] [b : Any]) 1 "b")
          ((structure [a] [b]) 1 "b")
          ((structure a b) 1 "b")
          ((structure [a] b) 1 "b")]
         [(structure [a "1"] [b 'b])
          (structure [a : String "1"] [b : Symbol 'b])
          (structure [a : Any "1"] [b : Any 'b])]
         [st2]
         [sta]
         [st3])))
  
  ;; define-structure
  (begin
    (define-structure empty-st)
    (define-structure st [a Number] [b String])
    (define-structure st2 [b String] [a Number] #:? custom-is-st2?)
    (define-structure st3 [c String] [a Number] #:? custom-is-st3?))
  
  ;; Constructor:
  ;; BUG 137 (check-equal?: (empty-st) : empty-st (empty-st))
  (begin
    (check-equal?: (structure-get (st 1 "b") b) : String "b")
    (check-equal?: (structure-get (st2 "a" 2) b) : String "a"))
  
  ;; Constructor, as id:
  (begin
    (check-equal?: (structure-get (cadr (map st '(1 2 3) '("x" "y" "z"))) b)
                   : String
                   "y")
    (check-equal?: (structure-get (cadr (map st2 '("d" "e" "f") '(1 2 3))) b)
                   : String
                   "e"))
  
  ;; type-expander
  (begin
    (check-equal?: (structure-get (ann (st2 "g" 123) st2) b) "g"))
  
  ;; match-expander
  (begin
    (check-equal?: (match (st2 "h" 7) [(st x y) (cons x y)])
                   : (Pairof Number String)
                   '(7 . "h")))
  
  ;; Equality
  (begin
    ;; BUG 137 (check-equal?: (ann (st 1 "i") st) (st 1 "i"))
    ;; BUG 137 (check-equal?: (ann (st2 "j" 2) st2) (st2 "j" 2))
    ;; BUG 137 (check-equal?: (ann (st 1 "k") st) (st2 "k" 1))
    )
  
  ;; Predicate
  (begin
    (check-equal?: (st? (ann (st 1 "i") (U st st2))) #t)
    (check-equal?: (custom-is-st2? (ann (st 1 "i") (U st st2))) #t)
    (check-equal?: (custom-is-st3? (ann (st 1 "i") (U st st2))) #f)
    (check-equal?: (st? (ann (st 1 "i") (U Number st st2))) #t)
    (check-equal?: (st? (ann 1 (U Number st st2))) #f)
    ;; Occurrence typing won't work well, if only because fields could be of
    ;; a type for which TR doesn't know how to make-predicate.
    #|(define (check-occurrence-typing [x : (U Number st st3)])
       (if (st? x)
       (match (ann x st) [(st the-a the-b) (cons the-b the-a)])
       'other))
       (check-equal?
       (check-occurrence-typing (ann (st 1 "i") (U Number st st3)))
       '("i" . 1))
       (check-equal?
       (check-occurrence-typing (ann (st2 "j" 2) (U Number st st3)))
       'other)
       (check-equal?
       (check-occurrence-typing (ann 9 (U Number st st3)))
       'other)|#))