#lang debug typed/racket

(require (submod "graph3.lp2.rkt" test))
(require racket/list)

;#R(force (second g))

(ann g 1)

#|
#R(force (caadr (force (car (second g)))))
#R(map force (second g))

#R(map force (third g))
|#