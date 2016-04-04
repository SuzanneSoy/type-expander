#lang typed/racket

(module test typed/racket
  (require "queue.lp2.rkt"
           typed/rackunit)
  
  (let-values
      ([(h t _)
        ((inst fold-queue-sets-immutable-tags
               Integer
               Void
               String
               (List 'a Integer String))
         (set 6 7)
         (void)
         (λ (e acc) (values (format "{~a}" e) acc))
         (λ (e acc x get-tag)
           (let*-values ([(t1 acc1 x1) (get-tag (if (even? e)
                                                    (floor (/ e 2))
                                                    (+ (* 3 e) 1))
                                                acc
                                                x)]
                         [(t2 acc2 x2) (get-tag 85 acc1 x1)])
             (values (list 'a e t1) acc2 x2))))])
    (check-equal? (sort (hash-keys h) <)
                  (sort '(7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1
                            6 3
                            85 256 128 64 32)
                        <))
    (check-true (set=? (set-remove
                        (set-remove
                         (set-remove(list->set (hash-keys h)) 7)
                         6)
                        85)
                       (list->set
                        (map (λ ([x : (List 'a Integer String)])
                               (let ([s (caddr x)])
                                 (string->number
                                  (substring s 1 (- (string-length s)
                                                    1)))))
                             (hash-values h)))))))