#lang type-expander/lang
(require typed/rackunit)
(check-equal? (ann (add1 1)
                   (Let ([T Number]) T))
              2)