#lang typed/racket

(require (submod "graph3.lp2.rkt" test))
(require racket/list)

(force (car (second g)))
;(map force (second g))

;(map force (third g))
