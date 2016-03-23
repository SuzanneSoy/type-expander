#lang typed/racket

(module test typed/racket
  (require typed/rackunit)
  (require "cond-abort.rkt")
  
  ;; Type Checker: Incomplete case coverage in: (cond-abort)
  ;(cond-abort)
  
  (check-equal? (cond-abort [else 1]) 1)
  (check-equal? (cond-abort [#f 0] [else 1]) 1)
  
  (check-equal? (cond-abort [#t 2]) 2)
  (check-equal? (cond-abort [#f 0] [#t 2]) 2)
  
  (check-equal? (cond-abort [#t 'continue]
                            [#f (typecheck-fail #'"We should never get here")]
                            [#t 3])
                3)
  
  (check-equal?
   (cond-abort
    [#t 'continue]
    [#t (let ([ret (cond-abort
                    [#t 'continue]
                    [#f (typecheck-fail #'"We should never get here")]
                    [#t 'break]
                    [#t (typecheck-fail #'"We should never get here")]
                    [#t 'continue]
                    [#t (typecheck-fail #'"We should never get here")])])
          (ann ret 'continue))]
    [#t 4])
   4)
  
  (check-equal?
   (ann (let ([f (λ ([x : Integer])
                   (cond-abort
                    [#t (if (< x 3) 'continue x)]
                    [#f (typecheck-fail #'"We should never get here")]
                    [#t 4]
                    [else (typecheck-fail #'"We should never get here")]))])
          (list (f 2) (f 7)))
        (Listof Positive-Integer))
   '(4 7))
  
  (check-equal?
   (match-abort '(1 2 3)
     [(cons a b)
      (match-abort b
        [(list x y z) 'one]
        [(cons x y) 'break]
        [_ (typecheck-fail #'"We should never get here")])]
     [(list a b c)
      'two])
   'two))