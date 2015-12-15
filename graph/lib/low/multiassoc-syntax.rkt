#lang racket

(require syntax/parse
         syntax/parse/experimental/template
         syntax/stx)

(provide multiassoc-syntax
         cdr-assoc-syntax
         tmpl-cdr-assoc-syntax)

(define (multiassoc-syntax query alist)
  (map stx-cdr
       (filter (λ (xy) (free-identifier=? query (stx-car xy)))
               (syntax->list alist))))

(define (cdr-assoc-syntax query alist)
  (stx-cdr (findf (λ (xy) (free-identifier=? query (stx-car xy)))
                  (syntax->list alist))))

(define-template-metafunction (tmpl-cdr-assoc-syntax stx)
  (syntax-parse stx
    [(_ query [k . v] …)
     (cdr-assoc-syntax #'query #'([k . v] …))]))
