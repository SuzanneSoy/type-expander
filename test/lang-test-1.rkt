#lang type-expander
(require typed/rackunit)
(check-equal? (ann (add1 1)
                   (Let ([T Number]) T))
              2)