#lang typed/racket

#|
(let ([res-zero '()]
      [res-one '()]
      [res-two '()]
|#

(define (process-zero v res-zero)
  (cons v res-zero))

(define (enqueue [v : Integer])
  (let ([name : (U 'zero 'one 'two) (cond [(= (modulo v 3) 0) 'zero]
                                          [(= (modulo v 3) 1) 'one]
                                          [else 'two])])
    (cond [(eq? name 'zero) ]
          [(eq? name 'one) ]
          [(eq? name 'two) ])))