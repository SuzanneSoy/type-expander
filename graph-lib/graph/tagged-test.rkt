#lang typed/racket

(module test typed/racket
  (require "tagged.lp2.rkt"
           "../lib/low.rkt"
           "../type-expander/type-expander.lp2.rkt")
  
  (check-equal?: (match (ann (tagged t1 [x 1] [y "b"])
                             (tagged t1 [x : Number] [y : String]))
                   [(tagged t1 [x a] [y b]) (list 'ok b a)]
                   [_ #f])
                 '(ok "b" 1))
  (check-equal?: (match (ann (tagged foo [x "o"] [y 3] [z 'z])
                             (tagged foo
                                     [x String]
                                     [z 'z]
                                     [y Fixnum]))
                   [(tagged foo z x y) (list z y x)])
                 '(z 3 "o"))

  (define-type ma (tagged ma (fav String) (faa ma) (fab mb)))
  (define-type mb (tagged mb (fbv String) (fba ma))))