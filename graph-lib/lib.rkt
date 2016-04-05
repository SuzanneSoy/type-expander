#lang typed/racket

(require racket/require)

(define-syntax-rule (r/p . mods)
  (begin
    (require . mods)
    (provide (all-from-out . mods))))

(r/p phc-toolkit
     "type-expander/multi-id.lp2.rkt"
     "type-expander/type-expander.lp2.rkt"
     "graph/adt.lp2.rkt"
     ;"graph/graph.lp2.rkt"
     "graph/graph-6-rich-returns.lp2.rkt"
     "graph/get.lp2.rkt"
     "graph/map.rkt"
     #|"graph/rewrite-type.lp2.rkt"|#)

(require (subtract-in "graph/dotlang.rkt"
                      "type-expander/type-expander.lp2.rkt"))

(provide (all-from-out "graph/dotlang.rkt"))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))