#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Easy declararation of new identifiers with
 type-expander, match-expander, …}

@section{@racket[multi-id]}

TODO: maybe we should cache @tc[p-else] and @tc[p-get].

@chunk[<fail-set!>
       #'(raise-syntax-error
          'self
          (format "can't set ~a" (syntax->datum #'self)))]

@CHUNK[<stx-class-kw-else>
       (define-splicing-syntax-class kw-else
         #:attributes (p-just-set! p-just-call p-just-id)
         (pattern (~seq #:mutable-else p-else)
                  #:with p-just-set! #'#'(set! p-else . rest)
                  #:with p-just-call #'#'(p-else . rest)
                  #:with p-just-id #'#'p-else)
         (pattern (~seq #:else p-else)
                  #:with p-just-set! <fail-set!>
                  #:with p-just-call #'#`(#,p-else . rest)
                  #:with p-just-id #'p-else)
         (pattern (~seq #:mutable-else-id p-else-id)
                  #:with (:kw-else) #'(#:mutable-else #'p-else-id))
         (pattern (~seq #:else-id p-else-id)
                  #:with (:kw-else) #'(#:else #'p-else-id)))]

@chunk[<stx-class-kw-set!+call+id>
       (define-splicing-syntax-class kw-set!+call+id
         (pattern (~seq (~optional (~seq #:set! p-user-set!:expr))
                        (~optional (~or (~seq #:call p-user-call:expr)
                                        (~seq #:call-id p-user-call-id:id)))
                        (~optional (~seq #:id p-user-id:expr)))
                  ; TODO: add #:macro with prop:procedure, see
                  ; file:///usr/local/racket-6.3.0.4/doc/syntax/stxparse-patter
                  ; ns.html?q=~optional#%28def._%28%28lib._syntax%2Fparse..rkt%
                  ; 29._prop~3apattern-expander%29%29
                  #:attr p-just-set!
                  (and (attribute p-user-set!) #'(p-user-set! stx))
                  #:attr p-just-call
                  (cond [(attribute p-user-call)
                         #'(p-user-call stx)]
                        [(attribute p-user-call-id)
                         #'(syntax-case stx ()
                             [(_ . rest) #'(p-user-call-id . rest)])]
                        [else #f])
                  #:attr p-just-id
                  (and (attribute p-user-id) #'(p-user-id stx))))]

Since we have an issue with the type-expander and recursive types (it goes in an
infinite loop), we temporarily provide a workaround with the
@tc[#:type-noexpand] and @tc[#:type-expand-once] keywords.

@chunk[<multi-id>
       (require (only-in typed/racket [define-type tr:define-type]))
       
       (begin-for-syntax
         <stx-class-kw-else>
         <stx-class-kw-set!+call+id>)
       (define-syntax/parse
         (define-multi-id name:id
           (~optional (~or (~seq #:type-expander p-type:expr)
                           (~seq #:type-noexpand p-type-noexpand:expr)
                           (~seq #:type-expand-once p-type-expand-once:expr)))
           (~optional (~or (~seq #:match-expander p-match:expr)
                           (~seq #:match-expander-id p-match-id:id)))
           (~optional (~seq #:custom-write p-write:expr))
           (~or (~seq #:set!-transformer p-set!:expr)
                :kw-else
                :kw-set!+call+id))
         (template
          (begin
            (?? (tr:define-type name p-type-noexpand #:omit-define-syntaxes))
            (?? (define-type name p-type-expand-once #:omit-define-syntaxes))
            (define-syntax name
              (let ()
                (struct tmp ()
                  (?? (?@ #:property prop:type-expander p-type))
                  (?? (?@ #:property prop:match-expander p-match))
                  (?? (?@ #:property prop:match-expander
                          (λ (stx) (syntax-case stx ()
                                     [(_ . rest) #'(p-match-id . rest)]))))
                  (?? (?@ #:property prop:custom-write p-write))
                  #:property prop:set!-transformer
                  (?? p-set!
                      (λ (_ stx)
                        (syntax-case stx (set!)
                          [(set! self . rest) (?? p-set! <fail-set!>)]
                          (?? [(_ . rest) p-just-call])
                          (?? [_ p-just-id])))))
                (tmp))))))]

@chunk[<test-multi-id>
       (define (p1 [x : Number]) (+ x 1))
       
       (define-type-expander (Repeat stx)
         (syntax-case stx ()
           [(_ t n) #`(List #,@(map (λ (x) #'t)
                                    (range (syntax->datum #'n))))]))
       
       (define-multi-id foo
         #:type-expander
         (λ (stx) #'(List (Repeat Number 3) 'x))
         #:match-expander
         (λ (stx) #'(vector _ _ _))
         #:custom-write
         (λ (self port mode) (display "custom-write for foo" port))
         #:set!-transformer
         (λ (_ stx)
           (syntax-case stx (set!)
             [(set! self . _)
              (raise-syntax-error 'foo (format "can't set ~a"
                                               (syntax->datum #'self)))]
             [(_ . rest) #'(+ . rest)]
             [_ #'p1])))
       
       (check-equal? (ann (ann '((1 2 3) x) foo)
                          (List (List Number Number Number) 'x))
                     '((1 2 3) x))
       
       ;(set! foo 'bad)
       
       (let ([test-match (λ (val) (match val [(foo) #t] [_ #f]))])
         (check-equal? (test-match #(1 2 3)) #t)
         (check-equal? (test-match '(1 x)) #f))
       
       (check-equal? (foo 2 3) 5)
       (check-equal? (map foo '(1 5 3 4 2)) '(2 6 4 5 3))]

It would be nice to test the @tc[(set! foo 'bad)] case, but grabbing the
compile-time error is a challenge (one could use @tc[eval], but it's a bit heavy
to configure).

Test with @tc[#:else]:

@chunk[<test-multi-id>
       (define-multi-id bar-id
         #:type-expander
         (λ (stx) #'(List `,(Repeat 'x 2) Number))
         #:match-expander
         (λ (stx) #'(cons _ _))
         #:custom-write
         (λ (self port mode) (display "custom-write for foo" port))
         #:else-id p1)
       
       (check-equal? (ann (ann '((x x) 79) bar)
                          (List (List 'x 'x) Number))
                     '((x x) 79))
       
       ;(set! bar 'bad)
       
       (let ([test-match (λ (val) (match val [(bar-id) #t] [_ #f]))])
         (check-equal? (test-match '(a . b)) #t)
         (check-equal? (test-match #(1 2 3)) #f))

       (let ([f-bar-id bar-id])
         (check-equal? (f-bar-id 6) 7))
       (check-equal? (bar-id 6) 7)
       (check-equal? (map bar-id '(1 5 3 4 2)) '(2 6 4 5 3))]

@chunk[<test-multi-id>
       (define-multi-id bar
         #:type-expander
         (λ (stx) #'(List `,(Repeat 'x 2) Number))
         #:match-expander
         (λ (stx) #'(cons _ _))
         #:custom-write
         (λ (self port mode) (display "custom-write for foo" port))
         #:else #'p1)
       
       (check-equal? (ann (ann '((x x) 79) bar)
                          (List (List 'x 'x) Number))
                     '((x x) 79))
       
       ;(set! bar 'bad)
       
       (let ([test-match (λ (val) (match val [(bar) #t] [_ #f]))])
         (check-equal? (test-match '(a . b)) #t)
         (check-equal? (test-match #(1 2 3)) #f))
       
       (check-equal? (bar 6) 7)
       (check-equal? (map bar '(1 5 3 4 2)) '(2 6 4 5 3))]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require "type-expander.lp2.rkt"
                    "../lib/low.rkt")
           (require (for-syntax
                     racket/syntax
                     syntax/parse
                     syntax/parse/experimental/template
                     (only-in "type-expander.lp2.rkt" prop:type-expander)))
           (provide define-multi-id)
           
           <multi-id>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    "type-expander.lp2.rkt"
                    typed/rackunit
                    (for-syntax racket/list))
           
           <test-multi-id>
           
           (require (submod ".." doc))))]