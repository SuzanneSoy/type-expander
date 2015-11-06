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

