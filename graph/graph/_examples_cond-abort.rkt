#lang typed/racket

(require "cond-abort.rkt")

(match-abort '(1 (a b) 3)
  [(list x y z)
   (let-abort ([new-x x]
               [new-y (match-abort y
                        [(list n p) (list 'A n p)]
                        [(list q r s) (list 'B q r s)])]
               [new-z z])
              (list new-x new-y new-z))])


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


(begin
  (:
   test1
   (→ (List (Pairof (List Symbol (Listof String)) String)) (List (Pairof (List Symbol (Listof Number)) Number))))
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
                       (match-abort
                           temp8
                         ((list String11 ...)
                          #;(map (λ ([String12 : String])
                                   (unprotect (match-abort String12 ((and String13) (protected (string-length String13))))))
                                 String11)
                          #;(map-abort
                             String11
                             String12
                             3 #;(match-abort String12 ((and String13) (protected (string-length String13)))))
                          
                          
                          
                          (let ([l String11])
                            (if (null? l)
                                ;; Special-case to avoid type inference issues with an empty
                                ;; result-list.
                                '()
                                (let ([result-list (list (let ([String12 (car l)]) 3))])
                                  (set! l (cdr l))
                                  (do : (U 'continue 'break (Listof Number)) ([stop : (U #f #t 'continue 'break)
                                             #f])
                                    (stop (if (eq? stop 'continue)
                                              'continue
                                              (if (eq? stop 'break)
                                                  'break
                                                  (reverse result-list))))
                                    (if (null? l)
                                        (set! stop #t)
                                        (let ([result (let ([String12 (car l)]) 3)])
                                          (if (or (eq? result 'continue) (eq? result 'break))
                                              (set! stop result)
                                              (begin
                                                (set! result-list (cons result result-list))
                                                (set! l (cdr l))))))))))
                          
                          
                          
                          
                          
                          
                          
                          ))))
                     (protected (list (unprotect Symbol9) (unprotect temp10)))))))
                (String6 (match-abort String4 ((and String14) (protected (string-length String14))))))
               (protected (cons (unprotect temp5) (unprotect String6))))))))
         (protected (list (unprotect temp2)))))))))