#lang racket

(require "low-untyped.rkt")
(sequence->list (in-last? (in-syntax #'(a b c))))
(sequence-length>= (in-syntax #'(a b c)) 1)