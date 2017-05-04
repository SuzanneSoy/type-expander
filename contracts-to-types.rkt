#lang type-expander

(provide :contract→type
         (rename-out [c→t contract→type]
                     [c→t contract->type]
                     [:contract→type :contract->type]))
(require racket/contract/base
         (for-syntax syntax/parse
                     type-expander/expander))

(define-type-expander c→t
  (syntax-parser
    [(_ ({~literal or/c} alt ...)) #'(U (c→t alt) ...)]
    [(_ ({~literal and/c} alt ...)) #'(∩ (c→t alt) ...)]
    [(_ ({~literal listof} c)) #'(Listof (c→t c))]
    [(_ ({~literal list/c} c ...)) #'(List (c→t c) ...)]
    [(_ ({~literal *list/c} prefix suffix ...)) #'(Rec R (U (Pairof prefix R)
                                                            (List suffix ...)))]
    [(_ ({~literal vectorof} c)) #'(Vectorof (c→t c))]
    [(_ ({~literal vector/c} c ...)) #'(Vector (c→t c) ...)]
    [(_ ({~literal cons/c} a d)) #'(Pairof (c→t a) (c→t d))]
    [(_ {~literal integer?}) #'Integer]
    [(_ {~literal string?}) #'String]
    [(_ {~literal symbol?}) #'Symbol]
    [(_ {~literal exact-nonnegative-integer?}) #'Exact-Nonnegative-Integer]
    [(_ {~literal exact-positive-integer?}) #'Exact-Positive-Integer]
    [(_ {~and τ ({~literal quote} _)}) #'τ]
    [(_ {~and τ {~or :number :str :id}}) #''τ]
    [(_ {~and τ ({~literal quasiquote} _)}) #'τ]
    [(_ ({~literal unquote} τ)) #'τ]
    [(_ c) (raise-syntax-error
        'contract→type
        (string-append
         "I cannot convert this contract to a type automatically."
         " Please fill in an issue at"
         " https://github.com/jsmaniac/type-expander/issues if the translation"
         " can easily be done automatically, or do the translation manually "
         " otherwise. "
         (format "~a" (syntax->datum #'c)))
        #'c)]))

(define-syntax (:contract→type stx)
  (syntax-case stx ()
    [(_ c) #`(writeln '#,(expand-type #`(c→t c)))]))