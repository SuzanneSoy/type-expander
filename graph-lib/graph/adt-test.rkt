#lang typed/racket

(module test typed/racket
  (require (submod "graph-test.rkt" test)
           "adt.lp2.rkt"
           phc-toolkit
           "../type-expander/type-expander.lp2.rkt")

  (define-tagged st2 [b String] [a Number])
  
  ((tagged t a b c) 1 'b "c")
  ((tagged t a [b] c) 1 'b "c")
  ((tagged t [a] [b] [c]) 1 'b "c")
  ((tagged t [a : Number] [b : Symbol] [c : String]) 1 'b "c")
  (tagged t [a : Number 1] [b : Symbol 'b] [c : String "c"])
  (tagged t [a 1] [b 'b] [c "c"])
  
  (tagged t [a 1] [b 'b] [c "c"])
  
  (define-tagged tabc [a 1] [b 'b] [c "c"]))