#lang racket

;; This file is derived from the one which can be found at:
;; https://github.com/soegaard/bracket/blob/master/docs/pr-math.rkt

(require "math-scribble/math-scribble.rkt")

(provide mathjax-source setup-math
         (all-from-out "math-scribble/math-scribble.rkt"))

(require scribble/html-properties
         scribble/base
         scribble/core)

(define mathjax-source
  "MathJax/MathJax.js?config=default"
  ;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  ; "http://c328740.r40.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=default"
  ;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-HTML"
  )

(define setup-math
  (compound-paragraph
   (style #f (list))
   (list
    (paragraph 
     (style 
      #f (list (alt-tag "script")
               (attributes `((type . "text/javascript")
                             (src . ,mathjax-source )))))
     '())
    (paragraph
     (style
      #f (list (alt-tag "script")
               (attributes '((type . "text/x-mathjax-config")))))
     "MathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$']]} });"))))
