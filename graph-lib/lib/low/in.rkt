#lang typed/racket

(provide in)

(require racket/stxparam)

(define-syntax-parameter in
  (λ _ "`in' used out of context. It can only be used in some forms."))