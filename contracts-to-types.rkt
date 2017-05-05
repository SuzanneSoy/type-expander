#lang type-expander

(provide :contract→type
         (rename-out [c→t contract→type]
                     [c→t contract->type]
                     [:contract→type :contract->type]))
(require (prefix-in c: (combine-in racket/base racket/contract/base))
         (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     type-expander/expander))

(begin-for-syntax
  (define-syntax-class arrow
    (pattern {~or {~literal ->} {~literal →} {~literal c:->}}))
  (define-syntax-class arrow*
    (pattern {~or {~literal ->*} {~literal c:->*}})))

(define-type-expander c→t
  (syntax-parser
    [(_ ({~literal c:or/c} alt ...)) #'(U (c→t alt) ...)]
    [(_ ({~literal c:and/c} alt ...)) #'(∩ (c→t alt) ...)]
    [(_ ({~literal c:listof} c)) #'(Listof (c→t c))]
    [(_ ({~literal c:list/c} c ...)) #'(List (c→t c) ...)]
    [(_ ({~literal c:*list/c} prefix suffix ...))
     #'(Rec R (U (Pairof (c→t prefix) R)
                 (List (c→t suffix) ...)))]
    [(_ ({~literal c:vectorof} c)) #'(Vectorof (c→t c))]
    [(_ ({~literal c:vector/c} c ...)) #'(Vector (c→t c) ...)]
    [(_ ({~literal c:cons/c} a d)) #'(Pairof (c→t a) (c→t d))]
    [(_ {~literal c:number?}) #'Number]
    [(_ {~literal c:integer?}) #'Integer]
    [(_ {~literal c:string?}) #'String]
    [(_ {~literal c:symbol?}) #'Symbol]
    [(_ {~literal c:char?}) #'Char]
    [(_ {~literal c:boolean?}) #'Boolean]
    [(_ {~literal c:bytes?}) #'Bytes]
    [(_ {~literal c:void?}) #'Void]
    [(_ {~literal c:exact-nonnegative-integer?}) #'Exact-Nonnegative-Integer]
    [(_ {~literal c:exact-positive-integer?}) #'Exact-Positive-Integer]
    [(_ ({~literal c:syntax/c} τ)) #'(Syntaxof (c→t τ))]
    [(_ ({~literal c:parameter/c} in)) #'(Parameterof (c→t in))]
    [(_ ({~literal c:parameter/c} in out)) #'(Parameterof (c→t in) (c→t out))]
    [(_ ({~literal c:promise/c} τ)) #'(Promise (c→t τ))]
    [(_ ({~literal c:suggest/c} τ)) #'(c→t τ)]
    [(_ ({~literal c:flat-rec-contract} R alt ...))
     #`(Rec R (U (c→t alt) ...))]
    [(_ (a:arrow {~seq {~optional kw:keyword}
                       {~and arg {~not {~literal ...}}}}
                 ...
                 rest {~and {~literal ...} ooo}
                 result))
     #:with rest-kw (datum->syntax #'here '#:rest #'ooo)
     #:with a* (datum->syntax #'here '->* #'a)
     (template (a* ((?@ (?? kw) (c→t arg)) ...)
                   rest-kw (c→t rest)
                   (c→t result)))]
    [(_ (a:arrow {~seq {~optional kw:keyword}
                       {~and arg {~not {~literal ...}}}}
                 ...
                 result))
     (template (a (?@ (?? kw) (c→t arg)) ... (c→t result)))]
    [(_ (a*:arrow* ({~seq {~optional mandatory-kw:keyword}
                          mandatory-arg}
                    ...)
                   {~optional
                    {~and opt
                          ({~seq {~optional optional-kw:keyword}
                                 optional-arg}
                           ...)}}
                   {~optional {~seq #:rest ({~literal c:listof} rest)}}
                   result))
     (quasitemplate (a* ((?@ (?? mandatory-kw) (c→t mandatory-arg)) ...)
                        #,@(if (attribute opt)
                               (template
                                {((?@ (?? optional-kw) (c→t optional-arg))
                                  ...)})
                               #'{})
                        (?? (?@ #:rest (c→t rest)))
                        (c→t result)))]
    [(_ {~literal c:any}) #'AnyValues]
    [(_ ({~literal c:values} v ...)) #'(Values (c→t v) ...)]
    [(_ {~and τ ({~literal quote} _)}) #'τ]
    [(_ {~and τ {~or :number :str :char :boolean}}) #''τ]
    [(_ {~and τ}) #:when (bytes? (syntax-e #'τ)) #''τ]
    [(_ {~and τ}) #:when (regexp? (syntax-e #'τ)) #''τ]
    [(_ {~and τ}) #:when (byte-regexp? (syntax-e #'τ)) #''τ]
    [(_ {~and τ ({~literal quasiquote} _)}) #'τ]
    [(_ ({~literal unquote} τ)) #'τ]
    [(_ v:id)
     ;; TODO: this is a terrible implementation. type-expander should provide
     ;; a way to attach information to an identifier, so that we can know that
     ;; v is a variable bound by flat-rec-contract.
     #'v]
    [(_ c) (raise-syntax-error
            'contract→type
            (string-append
             "I cannot convert this contract to a type automatically."
             " Please fill in an issue at"
             " https://github.com/jsmaniac/type-expander/issues if the"
             " translation can easily be done automatically, or do the"
             " translation manually otherwise. ")
            #'c)]))

(define-syntax (:contract→type stx)
  (syntax-case stx ()
    [(_ c) #`(writeln '#,(expand-type #`(c→t c)))]))