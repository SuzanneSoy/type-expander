#lang racket

(module m racket
  (require syntax/parse
           syntax/parse/experimental/template)
  (provide (rename-out [template syntax]
                       [quasitemplate quasisyntax])
           (all-from-out syntax/parse
                         syntax/parse/experimental/template)))

(require 'm)

(syntax-parse #'(a b)
  [(x (~optional y) z)
   #'(x (?? y 1) z)])