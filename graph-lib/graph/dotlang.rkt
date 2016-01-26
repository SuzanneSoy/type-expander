#lang typed/racket

(module dotlang racket
  (require typed/racket)
  
  (provide (except-out (all-from-out typed/racket)
                       #;#%top
                       #%module-begin)
           (rename-out #;[new-#%top #%top]
                       [new-#%module-begin #%module-begin]))
    
  
  (require "get.lp2.rkt"
           "../lib/low-untyped.rkt"
           (for-syntax racket/string
                       syntax/parse
                       racket/syntax
                       syntax/stx
                       syntax/strip-context
                       racket/struct
                       racket/function
                       syntax/srcloc
                       "../lib/low-untyped.rkt"))
  
  #|
  (define-syntax/parse (dot x:id)
    (let* ([str (symbol->string (syntax-e #'x))]
           [components (regexp-match* #px"([^.…]|\\.\\.+)+|…" str)]
           [unescaped (map (λ (m) (regexp-replace* #px"\\.(\\.+)" m "\\1"))
                           components)]
           [identifiers (map (λ (u) (datum->syntax #'x (string->symbol u)))
                             unescaped)]
           [leading-dot? (regexp-match #px"^(\\.|…)" str)]
           [trailing-dot? (regexp-match #px"\\.$" str)])
      (define/with-syntax (id …) identifiers)
      (cond
        [leading-dot? #'(λget id …)]
        [trailing-dot? (raise-syntax-error 'dot "Found trailing dot" #'x)]
        [else #'(get id …)])))

  (define-syntax (new-#%top stx)
    (syntax-case stx ()
      [(_  . x)
       (if (regexp-match #rx"\\." (symbol->string (syntax-e #'x)))
           #`(dot x)
           #'(#%top . x))]))|#
  
  (define-syntax (new-#%module-begin stx)
    (syntax-case stx ()
      [(_ . body)
       #`(#%module-begin
          . #,(fold-syntax replace-dots
                           #'body))]))
  
  (define-for-syntax (replace-dots stx)
    (syntax-parse stx
      [x:id
       #:when (regexp-match #px"^.*\\..*[^.]$"
                            (symbol->string (syntax-e #'x)))
       (let* ([str (symbol->string (syntax-e #'x))]
              [components (regexp-match* #px"([^.…]|\\.\\.+)+|…" str)]
              [unescaped (map (λ (m) (regexp-replace* #px"\\.(\\.+)" m "\\1"))
                              components)]
              [identifiers (map (λ (u) (datum->syntax stx
                                                      (string->symbol u)
                                                      stx
                                                      stx))
                                unescaped)]
              [leading-dot? (regexp-match #px"^(\\.|…)" str)]
              [trailing-dot? (regexp-match #px"\\.$" str)])
         (define/with-syntax (id …) identifiers)
         (if leading-dot?
             (let* ([loc (update-source-location stx #:span 1)])
               (quasisyntax/loc stx (#,(datum->syntax stx 'λget loc stx) id …)))
             (if (= (length identifiers) 1)
                 (quasisyntax/loc stx #,(car identifiers))
                 (quasisyntax/loc stx
                   (#,(datum->syntax stx 'get stx stx) id …)))))]
      [x:id
       #:when (regexp-match #px"\\.$" (symbol->string (syntax-e #'x)))
       (let* ([str (symbol->string (syntax-e #'x))]
              [unescaped (substring str 0 (sub1 (string-length str)))])
         (datum->syntax stx (string->symbol unescaped) stx stx))]
      [_ stx]))
  
  (define-for-syntax (fold-syntax f e)
    (cond
      [(syntax? e)
       (let ([new-e (f e)])
         (if (eq? new-e e)
             (datum->syntax e (fold-syntax f (syntax-e e)) e e)
             new-e))]
      [(pair? e)
       (cons (fold-syntax f (car e))
             (fold-syntax f (cdr e)))]
      [(vector? e)
       (list->vector (fold-syntax f (vector->list e)))]
      [(box? e)
       (box (fold-syntax f (unbox e)))]
      [(prefab-struct-key e)
       => (λ (k) (apply make-prefab-struct
                        k
                        (fold-syntax f (struct->list e))))]
      [else e])))

(require 'dotlang)
(provide (all-from-out 'dotlang))

(module test (submod ".." dotlang)
  (require typed/rackunit
           "../lib/low.rkt"
           "get.lp2.rkt"
           (submod "graph.lp2.rkt" test)
           "map.rkt")

  (let ((foo..bar 42))
    (check-equal?: foo..bar 42))
  
  (check-equal?: 'foo.bar '(get foo bar))
  
  ;; Srcloc tests:
  ;(let .a b) ;; Error on the first .
  ;(let .a.b b) ;; Error on the first .
  ;(let a.b b) ;; Error on the whole a.b
  
  (check-equal?: g.streets…houses…owner.name
                 : (Listof (Listof String))
                 (list (list "Amy" "Anabella") (list "Jack")))
  (check-equal?: (map: (curry map .owner.name) g.streets…houses)
                 : (Listof (Listof String))
                 (list (list "Amy" "Anabella") (list "Jack")))
  
  (define (slen [n : Index] [str : String])
    (check-equal?: (string-length str) n)
    (string->symbol str))
  
  (check-equal?: '(a . b) (cons 'a 'b))
  (check-equal?: '(a . b.c) (list 'a 'get 'b 'c))
  (check-equal?: '(a . b.c.d) (list 'a 'get 'b 'c 'd))
  (check-equal?: '(a.c . b) (cons (list 'get 'a 'c) 'b))
  (check-equal?: '(a.c.d . b) (cons (list 'get 'a 'c 'd) 'b))
  
  (check-equal?: '.aa.bb..cc.d (list 'λget 'aa (slen 5 "bb.cc") 'd))
  (check-equal?: '…aa...bb..cc.d (list 'λget '… (slen 9 "aa..bb.cc") 'd))
  (check-equal?: '…aa.….bb..cc.d (list 'λget '… 'aa '… (slen 5 "bb.cc") 'd))
  (check-equal?: '.aa.….bb..cc.d (list 'λget 'aa '… (slen 5 "bb.cc") 'd))
  (check-equal?: '.aa.….bb.cc.d (list 'λget 'aa '… 'bb 'cc 'd))
  (check-equal?: '…aa.….bb.cc.d (list 'λget '… 'aa '… 'bb 'cc 'd))
  
  (check-equal?: 'aa.bb..cc.d (list 'get 'aa (slen 5 "bb.cc") 'd))
  (check-equal?: 'aa...bb..cc.d (list 'get (slen 9 "aa..bb.cc") 'd))
  (check-equal?: 'aa…bb..cc.d (list 'get 'aa '… (slen 5 "bb.cc") 'd))
  (check-equal?: 'aa.….bb..cc.d (list 'get 'aa '… (slen 5 "bb.cc") 'd))
  (check-equal?: 'aa.….bb.cc.d (list 'get 'aa '… 'bb 'cc 'd))
  
  (check-equal?: '… (slen 1 "…"))
  
  #|
  (check-equal?: '…aa.…bb..cc.d) ;; TODO: should cause error
  (check-equal?: '…aa….bb..cc.d) ;; TODO: should cause error
  |#)
