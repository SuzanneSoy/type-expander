#lang debug typed/racket

;(require "structure.lp2.rkt")
;(require "variant.lp2.rkt")
;(require "../type-expander/type-expander.lp2.rkt")
(require "../lib/low.rkt")

#|

(define-structure st [a Number] [b String])
(check-equal:? (st 1 "b") (structure [a 1] [b "b"]))
(check-equal:? (st 1 "b") (structure [a : Number 1] [b : String "b"]))
(check-equal:? (st 1 "b") ((structure [a : Number] [b : String]) 1 "b"))
(check-equal:? (st 1 "b") ((structure [a] [b]) 1 "b"))
(check-equal:? (st 1 "b") ((structure a b) 1 "b"))
(check-equal:? (st 1 "b") ((structure [a] b) 1 "b"))

((tagged t a b c) 1 'b "c")
((tagged t a [b] c) 1 'b "c")
((tagged t [a] [b] [c]) 1 'b "c")
((tagged t [a : Number] [b : Symbol] [c : String]) 1 'b "c")
(tagged t [a : Number 1] [b : Symbol 'b] [c : String "c"])
(tagged t [a 1] [b 'b] [c "c"])

#|
(require (submod "graph3.lp2.rkt" test))
(require "../lib/low.rkt")
(require racket/list)

(define #:∀ (A) (map-force [l : (Listof (Promise A))])
  (map (inst force A) l))

(let ()
  (map-force (second g))
  (cars (map-force (second g)))
  (map-force (third g))
  (map-force (append* (cars (cdrs (cdrs (map-force (second g)))))))
  (void))

#|
#R(map-force (second g))
#R(map-force (third g))

(newline)

#R(force (car (second g)))
#R(force (cadr (force (car (caddr (force (car (second g))))))))

(newline)
;|#

(define (forceall [fuel : Integer] [x : Any]) : Any
  (if (> fuel 0)
      (cond [(list? x) (map (curry forceall fuel) x)]
            [(promise? x) (forceall (sub1 fuel) (force x))]
            [else x])
      x))

(forceall 5 g)

|#
|#