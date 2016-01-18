#lang typed/racket

#|
(require "structure.lp2.rkt")
(require "variant.lp2.rkt")
(require "../type-expander/type-expander.lp2.rkt")
(require "../lib/low.rkt")

((tagged t a b c) 1 'b "c")
((tagged t a [b] c) 1 'b "c")
((tagged t [a] [b] [c]) 1 'b "c")
((tagged t [a : Number] [b : Symbol] [c : String]) 1 'b "c")
(tagged t [a : Number 1] [b : Symbol 'b] [c : String "c"])
(tagged t [a 1] [b 'b] [c "c"])

(tagged t [a 1] [b 'b] [c "c"])

(define-tagged tabc t [a 1] [b 'b] [c "c"])
|#

(require "__DEBUG_require.rkt")

;; (map: (compose (curry map identity) (λget houses …)) (get g streets))
;; (map: (compose (λget houses …) (λ #:∀ (A) ([x : A]) x)) (get g streets))
;; (map: (compose (λget houses …) (curry map (λget owner))) (get g streets))
;; => TODO: Crashes ../type-expander/type-expander.lp2.rkt:90:7: syntax-local-bind-syntaxes: contract violation
;  expected: (listof identifier?)
;  given: (#<syntax:/home/georges/phc/racket/graph-lib/graph/map.rkt:130:18 (has-get Out houses …)>)
;  argument position: 1st
;  other arguments...:

#|
(get '((1 2) (3)) … …)
(structure-get (cadr (force g)) people)
(get g people)
(get g streets cadr houses car owner name)
((λget people) g)
((λget owner name) (get g streets cadr houses car))
(get g streets … houses … owner name)
((λget streets … houses … owner name) g)
(let ([f (λget streets … houses … owner name)]) f)
;(map: (λget houses … owner name) (get g streets))
|#


#|
(define #:∀ (A) (map-force [l : (Listof (Promise A))])
  (map (inst force A) l))

(map-force (get g people))
(map-force (get g streets))
|#

#|
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