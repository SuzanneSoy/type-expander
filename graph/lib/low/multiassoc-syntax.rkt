#lang racket

(require syntax/parse
         syntax/parse/experimental/template
         syntax/stx)

(provide multiassoc-syntax
         cdr-assoc-syntax
         tmpl-cdr-assoc-syntax)

(require "../low.rkt") ;; For the identifier "…"

;; TODO: cdr-stx-assoc is already defined in lib/low.rkt

(define (multiassoc-syntax query alist)
  (map stx-cdr
       (filter (λ (xy) (free-identifier=? query (stx-car xy)))
               (syntax->list alist))))

(define (cdr-assoc-syntax query alist)
  (stx-cdr (assoc-syntax query alist)))

(define (assoc-syntax query alist)
  (findf (λ (xy) (free-identifier=? query (stx-car xy)))
         (syntax->list alist)))

(define-template-metafunction (tmpl-cdr-assoc-syntax stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:default default)) query [k . v] …)
     (if (attribute default)
         (let ([r (assoc-syntax #'query #'([k . v] …))])
           (if r
               (stx-cdr r)
               #'default))
         (cdr-assoc-syntax #'query #'([k . v] …)))]))
