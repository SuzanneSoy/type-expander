#lang typed/racket

(require "cond-abort.rkt")
(require "variant.lp2.rkt")
(require typed/rackunit)

(check-equal?
 (match-abort '(1 (a b) 3)
   [(list x y z)
    (let-abort ([new-x x]
                [new-y (match-abort y
                         [(list n p) (list 'A n p)]
                         [(list q r s) (list 'B q r s)])]
                [new-z z])
               (list new-x new-y new-z))])
 '(1 (A a b) 3))

(let ()
  (λ ([x : (U (Vector Number) (Vector String String))])
    (if (= (vector-length x) 1)
        x ;; Occurrence typing didn't narrow the type of x to (Vector Number).
        x))
  (void))

#|

(λ ((v : (List Symbol String)))
  (match-abort
      v
    ((list Symbol1 String2)
     (let-abort
      ((Symbol3 (protected Symbol1)) (String4 (match-abort String2 ((and String5) (string-length String5)))))
      (list (unprotect Symbol3) (unprotect String4))))))

(λ ([v : (List Symbol String)])
  (match-abort
      v
    ((list Symbol1 String2)
     (let-abort
      ((Symbol3 #t) (String4 (match-abort String2 ((and String5) (string-length String5)))))
      (list Symbol3 String4)))))

(λ ((v : (List Symbol String)))
  (unprotect
   (match-abort
       v
     ((list Symbol1 String2)
      (let-abort
       ((Symbol3 (protected Symbol1)) (String4 (match-abort String2 ((and String5) (protected (string-length String5))))))
       (protected (list (unprotect Symbol3) (unprotect String4))))))))

|#



(check-equal?
 (foldl
  (λ (x acc)
    (if (null? x)
        acc;(reverse acc)
        (if (eq? x 'boo)
            'continue
            (cons x acc))))
  '()
  '(a b c))
 '(c b a))


(begin
  (:
   test1a
   (→
    (List (Pairof (List Symbol (Listof String)) String))
    (List (Pairof (List Symbol (Listof Number)) Number))))
  (define (test1a v)
    (let-values (((temp2) (apply values v)))
      (list
       (let ((val-cache3 temp2))
         (cons
          (let-values (((Symbol5 temp6) (apply values (car val-cache3))))
            (list Symbol5 (map (λ ((String9 : String)) (string-length String9)) temp6)))
          (string-length (cdr val-cache3))))))))



(begin
  (:
   test1
   (→
    (List
     (Pairof
      (U
       (List 'tag1 (List (Vector Symbol) (Listof String)))
       (List 'tag2 (List (Vector Symbol) (Listof String))))
      String))
    (List
     (Pairof
      (U
       (List 'tag1 (List (Vector Symbol) (Listof Number)))
       (List 'tag2 (List (Vector Symbol) (Listof Number))))
      Number))))
  (define (test1 v)
    (let-values (((temp2) (apply values v)))
      (list
       (let ((val-cache3 temp2))
         (cons
          (let ((val-cache4 (car val-cache3)))
            (cond
              ((and (list? val-cache4) (eq? 'tag1 (car val-cache4)))
               (let-values (((temp6 temp7) (apply values val-cache4)))
                 (list
                  temp6
                  (let-values (((temp10 temp11) (apply values temp7)))
                    (list
                     (let ((val-cache12 temp10))
                       (let ((Symbol13 (vector-ref val-cache12 0))) (vector Symbol13)))
                     (map (λ ((String16 : String)) (string-length String16)) temp11))))))
              ((and (list? val-cache4) (eq? 'tag2 (car val-cache4)))
               (let-values (((temp20 temp21) (apply values val-cache4)))
                 (list
                  temp20
                  (let-values (((temp24 temp25) (apply values temp21)))
                    (list
                     (let ((val-cache26 temp24))
                       (let ((Symbol27 (vector-ref val-cache26 0))) (vector Symbol27)))
                     (map (λ ((String30 : String)) (string-length String30)) temp25))))))))
          (string-length (cdr val-cache3))))))))

#|
(define-syntax-rule (map-abort lst v . body)
  #;(let ([l (foldl (λ (v acc)
                    (let ([result (let () . body)])
                      (if (eq? result 'continue)
                          'continue
                          (if (eq? result 'break)
                              'break
                              (cons (unprotect result) acc)))))
                  '()
                  lst)])
    (if (or (eq? l 'continue) (eq? l 'break))
        l
        (reverse l))))

(begin
  (:
   test1
   (→
    (List (Pairof (List Symbol (Listof String)) String))
    (List (Pairof (List Symbol (Listof Number)) Number))))
  (define (test1 v)
    (unprotect
     (match-abort
         v
       ((list temp1)
        (let-abort
         ((temp2
           (match-abort
               temp1
             ((cons temp3 String4)
              (let-abort
               ((temp5
                 (match-abort
                     temp3
                   ((list Symbol7 temp8)
                    (let-abort
                     ((Symbol9 (protected Symbol7))
                      (temp10
                       #;(match-abort
                             temp8
                           ((list String11 ...)
                            (begin String11 (error "e"))))
                       (match-abort
                           temp8
                         ((list String11 ...)
                          (map-abort
                           String11
                           String12
                           (match-abort
                               String12
                             ((and String13)
                              (protected (string-length String13)))))))))
                     (protected
                      (list (unprotect Symbol9) (unprotect temp10)))))))
                (String6
                 (match-abort
                     String4
                   ((and String14) (protected (string-length String14))))))
               (protected (cons (unprotect temp5) (unprotect String6))))))))
         (protected (list (unprotect temp2)))))))))
|#