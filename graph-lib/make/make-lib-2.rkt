#lang racket

;; TODO: use (or (file-exists? path) (directory-exists? path)
;; (link-exists? path)), when checking whether the output was created.

(require (for-syntax syntax/parse)
         rackunit)

(begin-for-syntax
  (define-splicing-syntax-class element
    (pattern (~seq as-list (~literal ...)))
    (pattern (~seq single-value)
             #:with as-list #'(list single-value))))

(define-match-expander path
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(app explode-path
              (list #;(and (or (? path-for-some-system?) 'up 'same) pat)
                    pat ...))]))
  (λ (stx)
    (syntax-parse stx
      [(_ e:element ...)
       #'(apply build-path (append e.as-list ...))])))

(define-match-expander simple-path
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(app (λ (x) (simplify-path x #f)) (path pat ...))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ e ...)
       #'(simplify-path (path e ...) #f)])))

(check-equal? (match (build-path "a/b/c/d/e/f" 'up "g/")
                ((path x y ... z) (path z y ... 'up x)))
              (string->path "g/b/c/d/e/f/../../a"))

(check-not-equal? (match (build-path "a/b/c/d/e/f" 'up "g/")
                    ((path x y ... z) (path z y ... 'up x)))
                  (string->path "g/b/c/d/a"))

(check-equal? (match (build-path "a/b/c/d/e/f" 'up "g/")
                ((simple-path x y ... z) (simple-path z y ... 'up x)))
              (string->path "g/b/c/d/a"))
