#lang typed/racket

(module test typed/racket
  (require (submod "graph-test.rkt" test))
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
  
  (define-tagged tabc [a 1] [b 'b] [c "c"]))