#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide % define%)

  (require (for-syntax syntax/parse))
  
  #|(define-syntax (% stx)
  (syntax-parse stx #:literals (= → :)
    [(_ (~seq (~or ((~and var (~not :)) ...)
                   (~seq (~and var (~not (~or = → :))) ...)) = expr)
        ...
        (~optional (~literal →)) . body)
     #'(let-values ([(var ...) expr] ...) . body)]))|#
  
  (begin-for-syntax
    (define-syntax-class %pat
      (pattern v:id
               #:with expanded #'v)
      (pattern ()
               #:with expanded #'(list))
      (pattern (x:%pat . rest:%pat)
               #:with expanded #'(cons x.expanded rest.expanded)))
    (define-splicing-syntax-class %assignment
      #:attributes ([pat.expanded 1] [expr 0])
      #:literals (= →)
      (pattern (~seq (~and maybe-pat (~not (~or = →))) ... (~datum =) expr:expr)
               #:with [pat:%pat ...] #'(maybe-pat ...))))
  
  (define-syntax (% stx)
    (syntax-parse stx #:literals (= →)
      [(_ :%assignment ... (~optional (~literal →)) . body)
       #'(match-let*-values ([(pat.expanded ...) expr] ...) . body)]))
  
  (begin-for-syntax
    (define-syntax-class typed-pat
      (pattern [x:%pat (~literal :) type:expr]
               #:with (tmp) (generate-temporaries #'(x))
               #:with var-type #`[tmp : type]
               #:with (expanded ...) #'([x.expanded tmp]))
      (pattern x:id
               #:with var-type #'x
               #:with (expanded ...) #'())
      (pattern x:%pat
               #:with (tmp) (generate-temporaries #'(x))
               #:with var-type #'tmp
               #:with (expanded ...) #'([x.expanded tmp]))))
  
  (define-syntax (define% stx)
    (syntax-parse stx
      [(_ (name param:typed-pat ...)
          (~and (~seq ret ...) (~optional (~seq (~literal :) ret-type)))
          . body)
       #'(define (name param.var-type ...)
           (match-let (param.expanded ... ...) ret ... . body))]))
  
  #|
  (begin-for-syntax
    (define-syntax-class λ%expr
      (pattern e:id #:where (symbol->string e))
      (pattern e)
      (pattern (e . rest:λ%expr))))
  
  (define-syntax (λ% stx)
    (syntax-parse stx
      [(_ expr )]))
  |#)