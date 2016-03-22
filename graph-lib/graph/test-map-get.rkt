#lang typed/racket

(module test typed/racket
  (require (submod "graph.lp2.rkt" test))
  (require "get.lp2.rkt")
  (require "map.rkt")
  (require "adt.lp2.rkt")
  (require "../lib/low.rkt")
  (require "../type-expander/type-expander.lp2.rkt")
  
  ((tagged t a b c) 1 'b "c")
  ((tagged t a [b] c) 1 'b "c")
  ((tagged t [a] [b] [c]) 1 'b "c")
  ((tagged t [a : Number] [b : Symbol] [c : String]) 1 'b "c")
  (tagged t [a : Number 1] [b : Symbol 'b] [c : String "c"])
  (tagged t [a 1] [b 'b] [c "c"])
  
  (tagged t [a 1] [b 'b] [c "c"])
  
  (define-tagged tabc [a 1] [b 'b] [c "c"])
  
  (map: (λget houses) (get g streets))
  (map: (λget houses … owner name) (get g streets))
  (map: (∘ (curry map (∘ (λget name) (λget owner))) (λget houses))
        (get g streets))
  (map: (∘ (curry map (∘ string-length (λget name) (λget owner))) (λget houses))
        (get g streets))
  
  (map: (compose (curry map identity) (λget houses …)) (get g streets))
  ;; Can be allowed by changing (→ (→ A B) A B) in a couple of places in map: to
  ;; a case→, but it's more heavy on the typechecker, and it's an uncommon case.
  ;(map: (compose (λget houses …) (λ #:∀ (A) ([x : A]) x)) (get g streets))
  (map: (compose (curry map (λget owner)) (λget houses …)) (get g streets))
  
  (get '((1 2) (3)) … …)
  (structure-get (cadr (force g)) people)
  (get g people)
  (get g streets cadr houses car owner name)
  ((λget people) g)
  ((λget owner name) (get g streets cadr houses car))
  (get g streets … houses … owner name)
  ((λget streets … houses … owner name) g)
  (let ([f (λget streets … houses … owner name)]) f)
  (map: (λget houses … owner name) (get g streets)))