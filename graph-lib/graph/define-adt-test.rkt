#lang typed/racket

(module test typed/racket
  (require "define-adt.lp2.rkt"
           "constructor.lp2.rkt"
           "tagged.lp2.rkt"
           phc-toolkit
           "../type-expander/type-expander.lp2.rkt")
  
  ;; define-tagged
  (begin
    (define-tagged tagged-s1)
    (define-tagged tagged-s2 [f Fixnum] [g String])
    (define-tagged tagged-s3 [g String] [f Fixnum])
    (define-tagged tagged-s4 [f Fixnum] [g String])
    
    (check-equal?: (match (ann (tagged-s1) (tagged tagged-s1))
                     [(tagged-s1) #t])
                   #t)
    
    (check-equal?: (match (ann (tagged-s2 99 "z") tagged-s2)
                     [(tagged-s2 f g) (cons g f)])
                   '("z" . 99))
    
    (let ()
      (check-equal?: (match (ann (tagged-s2 99 "in-let") tagged-s2)
                       [(tagged-s2 f g) (cons g f)])
                     '("in-let" . 99)))
    
    (define (test-match val)
      (match val
        [(tagged-s2 x y) (list 'found-s2 y x)]
        [(tagged-s3 x y) (list 'found-s3 y x)]
        [(tagged-s4 x y) (list 'found-s4 y x)]))
    
    (check-equal?:
     (test-match (ann (tagged-s2 2 "flob")
                      (tagged tagged-s2 [f Fixnum] [g String])))
     '(found-s2 "flob" 2))
    
    (check-equal?:
     (test-match (ann (tagged-s3 "flob" 2)
                      (tagged tagged-s3 [g String] [f Fixnum])))
     '(found-s3 2 "flob"))
    
    ;; g and f are inverted in the “ann”
    (check-equal?:
     (test-match (ann (tagged-s4 2 "flob")
                      (tagged tagged-s4 [g String] [f Fixnum])))
     '(found-s4 "flob" 2))
    
    (define (test-match-verbose val)
      (match val
        [(tagged tagged-s2 g [f y]) (list 'found-s2 g y)]
        [(tagged tagged-s3 [g y] f) (list 'found-s2 f y)]
        [(tagged tagged-s4 [f y] g) (list 'found-s2 g y)]))
    
    (check-equal?:
     (test-match (ann (tagged-s2 3 "flob")
                      (tagged tagged-s2 [f Fixnum] [g String])))
     '(found-s2 "flob" 3))
    
    ;; g and f are inverted in the “ann”
    (check-equal?:
     (test-match (ann (tagged-s3 "flob" 3)
                      (tagged tagged-s3 [f Fixnum] [g String])))
     '(found-s3 3 "flob"))
    
    (check-equal?:
     (test-match (ann (tagged-s4 3 "flob")
                      (tagged tagged-s4 [f Fixnum] [g String])))
     '(found-s4 "flob" 3))
    
    (check-not-equal?: (tagged-s2 4 "flob")
                       (tagged-s3 "flob" 4))
    (check-not-equal?: (tagged-s2 4 "flob")
                       (tagged-s4 4 "flob")))
  
  ;; define-constructor
  (begin
    (define-constructor c1)
    (define-constructor c2 Fixnum String)
    (define-constructor c3 Fixnum String)
    
    (check-equal?: (match (ann (c1) (constructor c1))
                     [(c1) #t])
                   #t)
    
    (check-equal?: (match (ann (c2 99 "z") c2)
                     [(c2 f g) (cons g f)])
                   '("z" . 99))
    
    (let ()
      (check-equal?: (match (ann (c2 99 "in-let") c2)
                       [(c2 f g) (cons g f)])
                     '("in-let" . 99)))
    
    (define (test-c-match val)
      (match val
        [(c1) (list 'found-c1)]
        [(constructor c2 x y z) (list 'found-c2-xyz z y x)]
        [(c2 x y) (list 'found-c2 y x)]
        [(c3 x y) (list 'found-c3 y x)]))
    
    (check-equal?:
     (test-c-match (ann (c2 2 "flob")
                        (constructor c2 Fixnum String)))
     '(found-c2 "flob" 2))
    
    (check-equal?:
     (test-c-match (ann (c3 2 "flob")
                        (constructor c3 Fixnum String)))
     '(found-c3 "flob" 2)))
  
  ;; define-tagged #:private
  (begin
    (define-syntax-rule (defp make mt)
      (begin
        (define-tagged txyz #:private #:? txyz?
          [a Number]
          [b String])
        
        (define (make) (txyz 1 "b"))
        
        (define (mt v)
          (match v
            ((txyz x y) (list 'macro y x))
            (_ #f)))))
    
    (defp make mt)
    
    (define-tagged txyz #:private #:? txyz?
      [a Number]
      [b String])
    
    (check-equal?: (match (make)
                     ((tagged txyz x y) (list 'out y x))
                     (_ #f))
                   #f)
    
    (check-equal?: (mt (tagged txyz [x 1] [y "b"]))
                   #f)
    
    (check-equal?: (mt (make))
                   '(macro "b" 1))
    
    (check-not-equal?: (make) (txyz 1 "b"))
    (check-equal?: (match (make)
                     ((txyz x y) (list 'out y x))
                     (_ #f))
                   #f)
    
    (check-equal?: (mt (txyz 1 "b"))
                   #f))
  
  ;; define-constructor #:private
  (begin
    (define-syntax-rule (defpc makec mtc)
      (begin
        (define-constructor cxyz #:private #:? cxyz? Number String)
        
        (define (makec) (cxyz 1 "b"))
        
        (define (mtc v)
          (match v
            ((cxyz x y) (list 'macro y x))
            (_ #f)))))
    
    (defpc makec mtc)
    
    (define-constructor cxyz #:private #:? cxyz? Number String)
    
    (check-equal?: (match (makec)
                     ((constructor cxyz e f) (list 'out f e))
                     (_ #f))
                   #f)
    
    (check-equal?: (mtc (constructor cxyz 1 "b"))
                   #f)
    
    (check-equal?: (mtc (makec))
                   '(macro "b" 1))
    
    (check-not-equal?: (makec) (cxyz 1 "b"))
    (check-equal?: (match (makec)
                     ((cxyz e f) (list 'out f e))
                     (_ #f))
                   #f)
    
    (check-equal?: (mtc (cxyz 1 "b"))
                   #f)))