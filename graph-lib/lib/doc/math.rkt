#lang at-exp racket

;; This file is derived from the one which can be found at:
;; https://github.com/soegaard/bracket/blob/master/docs/pr-math.rkt

(require "math-scribble/math-scribble.rkt")

(provide mathjax-source setup-math
         (all-from-out "math-scribble/math-scribble.rkt"))

(require scribble/html-properties
         scribble/latex-properties
         scribble/base
         scribble/core)

;; Other possible sources:
;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
;"http://c328740.r40.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=default"
;"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-HTML"
(define mathjax-source "MathJax/MathJax.js?config=default")

(require scriblib/render-cond)

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
    (cond-block
     [(and (or html))
      (paragraph
       (style
        #f (list (alt-tag "script")
                 (attributes '((type . "text/x-mathjax-config")))))
       "MathJax.Hub.Config({ tex2jax: {inlineMath: [['$','$']]} });")]
     [latex
      (paragraph
       (style
        #f (list (tex-addition (string->bytes/utf-8 @string-append{
       %\overfullrule=2cm
       \usepackage[scaled=0.7]{beramono}
       \usepackage{newunicodechar}
       \newunicodechar{ᵢ}{\ensuremath{_1}}
       
       \usepackage{xcolor}
       \hypersetup{
        unicode=true,
        colorlinks=true,
        linkcolor={red!50!white!50!black},
        citecolor={blue!50!black},
        urlcolor={blue!80!black},
       }
       }))))
       "")]
     [else (paragraph (style #f (list)) "")]))))
