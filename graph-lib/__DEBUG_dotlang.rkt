#lang s-exp "graph/dotlang.rkt"

(require "graph/graph4.lp2.rkt")
(require "graph/map4.rkt")
(require (submod "graph/graph3.lp2.rkt" test))
(require "type-expander/type-expander.lp2.rkt")
(require "graph/structure.lp2.rkt")
(require "graph/variant.lp2.rkt")
(require "lib/low.rkt")

#|
.aa.bb..cc.d
…aa...bb..cc.d
…aa.….bb..cc.d
.aa.….bb..cc.d
(newline)
aa.bb..cc.d
aa...bb..cc.d
aa…bb..cc.d
aa.….bb..cc.d
(newline)
…aa.…bb..cc.d ;; TODO: should cause error
…aa….bb..cc.d ;; TODO: should cause error
|#

g.streets…houses…owner.name
(map: (curry map (λget owner name)) g.streets…houses)