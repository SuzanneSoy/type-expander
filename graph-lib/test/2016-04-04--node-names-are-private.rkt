#lang s-exp "../lib.rkt"

;; Should not cause name collisions
(define a "boom")
(define m-a "boom")

(define-graph g
  ([a (f1a : String) (f2a : (~> m-b))]
   [b (f1b : String)])
  [(m-a [v : String])
   : a
   (a v (m-b (string-append "b=" v)))]
  [(m-b [v : String])
   : b
   (b v)])

;; Should not cause name collisions
;(define b "boom")
(define m-b "boom")
