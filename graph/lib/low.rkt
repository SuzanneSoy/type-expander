#lang typed/racket

(provide degub)
(: degub (∀ (T) (→ T T)))
(define (degub x) (display "degub:") (displayln x) x)

;; ==== low/typed-untyped-module.rkt ====

(require typed/untyped-utils)
(provide half-typed-module typed/untyped-prefix define-modules)

;; half-typed-module
(define-syntax-rule (typed-module m typed-language untyped-language . body)
  (module m typed-language . body))

(define-syntax-rule (untyped-module m typed-language untyped-language . body)
  (module m untyped-language . body))

(define-typed/untyped-identifier half-typed-module typed-module untyped-module)

#| ;; test: should work in no-check but not in typed:
(half-typed-module moo typed/racket typed/racket/no-check
  (: foo One)
  (define foo 2))
|#

;; typed/untyped-prefix
(define-syntax-rule
  (typed-typed/untyped-prefix [typed-prefix ...] [untyped-prefix ...] . rest)
  (typed-prefix ... . rest))

(define-syntax-rule
  (untyped-typed/untyped-prefix [typed-prefix ...] [untyped-prefix ...] . rest)
  (untyped-prefix ... . rest))

(define-typed/untyped-identifier typed/untyped-prefix
  typed-typed/untyped-prefix
  untyped-typed/untyped-prefix)

#|
;; test: should work in no-check but not in typed:
(typed/untyped-prefix
 [module moo2 typed/racket]
 [module moo2 typed/racket/no-check]
 (: foo One)
 (define foo 2))
|#

;; define-modules
(define-syntax define-modules
  (syntax-rules (no-submodule)
    [(_ ([no-submodule] [name lang] ...) . body)
     (begin (begin . body)
            (module name lang . body) ...)]
    [(_ ([name lang] ...) . body)
     (begin (module name lang . body) ...)]))

#|
;; TODO: tests: test with a macro and check that we can use it in untyped.
;; TODO: tests: test with two mini-languages with different semantics for some
;; function.
(define-modules ([foo typed/racket] [foo-untyped typed/racket/no-check])
  (provide x)
  (: x (→ Syntax Syntax))
  (define (x s) s))

(module test racket
  (require (submod ".." foo-untyped))
  (x #'a))
|#

;; ==== low/require-provide.rkt ====

(provide require/provide)

(define-syntax (require/provide stx)
  (syntax-case stx ()
    [(_ require-spec ...)
     #'(begin
         (require require-spec ...)
         (provide (all-from-out require-spec ...)))]))

(module+ test
  (require typed/rackunit)
  (module ma typed/racket
    (define require-provide-foo 7)
    (provide require-provide-foo))
  (module mb typed/racket
    (require (submod ".." ".."))
    (require/provide (submod ".." ma)))
  (require 'mb)
  (check-equal? require-provide-foo 7))

;; ==== low/define-syntax-parse.rkt ====
(require syntax/parse
         syntax/parse/define)

(provide define-syntax/parse
         λ/syntax-parse)

(begin-for-syntax
  (require (for-syntax racket/base
                       racket/stxparam)
           racket/stxparam)
  
  (provide stx)
  
  (define-syntax-parameter stx
    (lambda (stx)
      (raise-syntax-error (syntax-e stx) "Can only be used in define-syntax/parse"))))

(define-simple-macro (define-syntax/parse (name . args) . body)
  (define-syntax (name stx2)
    (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
                         (syntax-parse stx2
                           [(_ . args) . body]))))

(define-simple-macro (λ/syntax-parse args . body)
  (λ (stx2)
    ;(syntax-parameterize ([stx (make-rename-transformer #'stx2)])
    (syntax-parse stx2
      [args . body])));)

;; If you include this as a file, you need to do:
;(begin-for-syntax (provide stx))
;; It's not provided by (all-from-out) :-(

;; ==== low/check-type-and-equal.rkt ====
(require ;"define-syntax-parse.rkt"
  (for-syntax syntax/parse
              syntax/parse/experimental/template)
  typed/rackunit)

(provide check-equal:?)

;; TODO: this won't expand types in the ann.

(define-syntax/parse
  (check-equal:? actual
                 (~optional (~seq (~datum :) type))
                 expected)
  (template (check-equal? (?? (ann actual type) actual) expected)))

;; ==== low/typed-fixnum.rkt ===

(provide fxxor)

;; For fxxor, used to compute hashes.
;; The type obtained just by writing (require racket/fixnum) is wrong, so we get a more precise one.
(require/typed racket/fixnum [(fxxor fxxor2) (→ Fixnum Fixnum Fixnum)])

(: fxxor (→ Fixnum * Fixnum))
(define (fxxor . args)
  (foldl fxxor2 0 args))

(module+ test
  (require typed/rackunit)
  (check-equal? (fxxor2 13206 23715) 28469)
  (check-equal? (fxxor 0) 0)
  (check-equal? (fxxor 13206) 13206)
  (check-equal? (fxxor 13206 23715 314576) 304101))

;; ==== Rest ====
(provide hash-set**
         map+fold
         cons→values
         (rename-out [cons→values cons->values])
         nameof
         first-value second-value third-value fourth-value fifth-value sixth-value seventh-value eighth-value ninth-value tenth-value
         (rename-out [compose ∘])
         stx-list
         stx-e
         stx-pair
         ;string-set!
         ;string-copy!
         ;string-fill!
         with-output-file
         in-tails
         in-heads
         in-split
         in-split*
         *in-split
         my-in-syntax
         indexof
         Syntax-Listof
         check-duplicate-identifiers
         generate-temporary)

(require (for-syntax syntax/parse syntax/parse/experimental/template))

(: hash-set** (∀ (K V)
                 (→ (HashTable K V) (Listof (Pairof K V)) (HashTable K V))))
(define (hash-set** h l)
  (if (null? l)
      h
      (hash-set** (hash-set h (caar l) (cdar l)) (cdr l))))


(define #:∀ (A B) (cons→values [x : (Pairof A B)]) (values (car x) (cdr x)))

(: map+fold (∀ (E R A) (→ (→ E A (values R A)) A (Listof E)
                          (Values (Listof R) A))))
(define (map+fold f init-acc lst)
  (let ([result (foldl (λ ([item : E] [acc : (Pairof (Listof R) A)])
                         (let-values ([(item new-acc) (f item (cdr acc))])
                           (cons (cons item (car acc))
                                 new-acc)))
                       (cons '() init-acc)
                       lst)])
    (values (car result) (cdr result))))

(define-syntax-rule (nameof x) (begin x 'x))

(module+ test
  (require typed/rackunit)
  (let ((y 3))
    (check-equal? (nameof y) 'y)))

;(define (raise-multi-syntax-error name message exprs)
;(let ([e (exn:fail:syntax "message" (current-continuation-marks) (list #'aaa #'bbb))])
;  ((error-display-handler) (exn-message e) e))

(define-syntax-rule (λstx (param ...) body ...)
  (λ (param ...)
    (with-syntax ([param param] ...)
      body ...)))

(module+ test
  (require typed/rackunit)
  (check-equal? (syntax->datum ((λstx (foo bar) #'(foo bar)) #'a #'b))
                (syntax->datum #'(a b))))

(define-syntax-rule (define-value-getter name v ... last-v)
  (define-syntax-rule (name expr)
    (call-with-values (λ () expr) (λ (v ... last-v . rest) last-v))))

(define-value-getter first-value   v1)
(define-value-getter second-value  v1 v2)
(define-value-getter third-value   v1 v2 v3)
(define-value-getter fourth-value  v1 v2 v3 v4)
(define-value-getter fifth-value   v1 v2 v3 v4 v5)
(define-value-getter sixth-value   v1 v2 v3 v4 v5 v6)
(define-value-getter seventh-value v1 v2 v3 v4 v5 v6 v7)
(define-value-getter eighth-value  v1 v2 v3 v4 v5 v6 v7 v8)
(define-value-getter ninth-value   v1 v2 v3 v4 v5 v6 v7 v8 v9)
(define-value-getter tenth-value   v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

(module+ test
  (require typed/rackunit)
  (check-equal? (first-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 1)
  (check-equal? (second-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 2)
  (check-equal? (third-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 3)
  (check-equal? (fourth-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 4)
  (check-equal? (fifth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 5)
  (check-equal? (sixth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 6)
  (check-equal? (seventh-value (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 7)
  (check-equal? (eighth-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 8)
  (check-equal? (ninth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 9)
  (check-equal? (tenth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 10))

(define-match-expander stx-list
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(? syntax?
            (app syntax->list (list pat ...)))])))

(module+ test
  (require typed/rackunit)
  (check-equal? (match #'(1 2 3) [(stx-list a b c) (list (syntax-e c)
                                                         (syntax-e b)
                                                         (syntax-e a))])
                '(3 2 1))
  
  (check-equal? (match #'(1 2 3)
                  [(stx-list a ...) (map (inst syntax-e Positive-Byte) a)])
                '(1 2 3))
  
  #;(check-equal? (match #`(1 . (2 3)) [(stx-list a b c) (list (syntax-e c)
                                                               (syntax-e b)
                                                               (syntax-e a))])
                  '(3 2 1)))

(define-match-expander stx-e
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat)
       #'(? syntax?
            (app syntax-e pat))])))

(module+ test
  (require typed/rackunit)
  (check-equal? (match #'x [(stx-e s) s]) 'x)
  (check-equal? (match #'(x . y) [(stx-e (cons a b)) (cons (syntax-e b)
                                                           (syntax-e a))])
                '(y . x)))

(define-match-expander stx-pair
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat-car pat-cdr)
       #'(? syntax?
            (app syntax-e (cons pat-car pat-cdr)))])))

(module+ test
  (require typed/rackunit)
  (check-equal? (match #'(x . y) [(stx-pair a b) (cons (syntax-e b)
                                                       (syntax-e a))])
                '(y . x))
  (check-equal? (match #'(x y z) [(stx-pair a b) (cons (map syntax->datum b)
                                                       (syntax->datum a))])
                '((y z) . x)))

(define-syntax (string-set! stx)
  (raise-syntax-error 'string-set! "Do not mutate strings." stx))
(define-syntax (string-copy! stx)
  (raise-syntax-error 'string-copy! "Do not mutate strings." stx))
(define-syntax (string-fill! stx)
  (raise-syntax-error 'string-fill! "Do not mutate strings." stx))

#|
(define-syntax (with-output-file stx)
  (syntax-parse stx
    [(_ filename:expr (~optional (~seq #:mode mode:expr)) (~optional (~seq #:exists exists:expr)) body ...)
     (template (with-output-to-file filename
                 (λ () body ...)
                 (?? (?@ #:mode mode))
                 (?? (?@ #:exists exists))))]))
|#

(define-syntax (with-output-file stx)
  (syntax-parse stx
    [(_ [var:id filename:expr] (~optional (~seq #:mode mode:expr)) (~optional (~seq #:exists exists:expr)) body ...)
     (template (call-with-output-file filename
                 (λ (var) body ...)
                 (?? (?@ #:mode mode))
                 (?? (?@ #:exists exists))))]))

(: in-tails (∀ (T) (→ (Listof T) (Listof (Pairof T (Listof T))))))
(define (in-tails l)
  (if (null? l)
      '()
      (cons l (in-tails (cdr l)))))

(module+ test
  (require typed/rackunit)
  (check-equal? (for/list : (Listof (Listof Number))
                  ([x : (Listof Number) (in-tails '(1 2 3 4 5))]) x)
                '((1 2 3 4 5) (2 3 4 5) (3 4 5) (4 5) (5)))
  (let ((l '(1 2 3 4 5)))
    (check-true (eq? (caddr (for/list : (Listof (Listof Number))
                              ([x : (Listof Number) (in-tails l)]) x))
                     (cddr l)))))

(: in-heads (∀ (T) (→ (Listof T) (Listof (Pairof T (Listof T))))))
(define (in-heads l)
  (: my-append1 (→ (Listof T) T (Pairof T (Listof T))))
  (define (my-append1 x y)
    (if (null? x)
        (list y)
        (cons (car x) (my-append1 (cdr x) y))))
  
  (define (on-heads/private [acc-head : (Listof T)] [l : (Listof T)])
    : (Listof (Pairof T (Listof T)))
    (if (null? l)
        '()
        (let ([new-head (my-append1 acc-head (car l))])
          (cons new-head (on-heads/private new-head (cdr l))))))
  (on-heads/private '() l))

(module+ test
  (require typed/rackunit)
  (check-equal? (for/list : (Listof (Listof Number))
                  ([x : (Listof Number) (in-heads '(1 2 3 4 5))]) x)
                '((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5))))

;; Can't write the type of on-split, because typed/racket doesn't allow writing (Sequenceof A B), just (Sequenceof A).
;; in-parallel's type has access to the multi-valued version of Sequenceof, though, so we let typed/racket propagate the inferred type.
(define #:∀ (T) (in-split [l : (Listof T)])
  (in-parallel (sequence-append (in-value '()) (in-heads l))
               (sequence-append (in-tails l) (in-value '()))))

;; Same as in-split, but without the empty tail.
(define #:∀ (T) (in-split* [l : (Listof T)])
  (in-parallel (sequence-append (in-value '()) (in-heads l))
               (sequence-append (in-tails l))))

;; Same as in-split, but without the empty head.
(define #:∀ (T) (*in-split [l : (Listof T)])
  (in-parallel (in-heads l)
               (sequence-append (sequence-tail (in-tails l) 1) (in-value '()))))

(define #:∀ (T) (*in-split* [l : (Listof T)])
  (in-parallel (in-heads l)
               (sequence-tail (in-tails l) 1)))

(: indexof (∀ (A B) (→ A (Listof B) (→ A B Any) (U #f Integer))))
(define (indexof elt lst [compare equal?])
  (let rec ([lst lst] [index 0])
    (if (null? lst)
        #f
        (if (compare elt (car lst))
            index
            (rec (cdr lst) (+ index 1))))))

;; See also syntax-e, which does not flatten syntax pairs, and syntax->list, which isn't correctly typed (won't take #'(a . (b c d e))).
(define-type (Syntax-Listof T)
  (Rec R (Syntaxof (U Null
                      (Pairof T R)
                      (Listof T)))))

;; in-syntax is now provided by racket/sequence.
(: my-in-syntax (∀ (T) (→ (Syntax-Listof T)
                          (Listof T))))
(define (my-in-syntax stx)
  (let ((e (syntax-e stx)))
    (if (null? e)
        e
        (if (syntax? (cdr e))
            (cons (car e) (my-in-syntax (cdr e)))
            e))))

(define (test-in-syntax)
  (my-in-syntax #'((a . b) (c . d))) ; (ann `(,#'(a . b) ,#'(c . d)) (Listof (Syntaxof (U (Pairof (Syntaxof 'a) (Syntaxof 'b)) (Pairof (Syntaxof 'c) (Syntaxof 'c))))))
  (my-in-syntax #'(a . (b c d e)))   ; (ann `(,#'a ,#'b ,#'c ,#'d ,#'e) (Listof (Syntaxof (U 'a 'b 'c 'd))))
  (my-in-syntax #'()))               ; (ann '() (Listof (Syntaxof Nothing)))


(: check-duplicate-identifiers (→ (Syntaxof (Listof (Syntaxof Symbol)))
                                  Boolean))
(define (check-duplicate-identifiers ids)
  (if (check-duplicate-identifier (my-in-syntax ids)) #t #f))

(require/typed racket/syntax [generate-temporary (→ Syntax Identifier)])

(require syntax/parse/define)
(provide define-simple-macro)

(require racket/match)
(provide (all-from-out racket/match)
         (rename-out [match-lambda match-λ]
                     [match-lambda* match-λ*]
                     [match-lambda** match-λ**]))


;; ==== ids.rkt ====

(define-modules ([no-submodule] [ids-untyped typed/racket/no-check])
  (provide format-ids
           hyphen-ids
           format-temp-ids;
           #|t/gen-temp|#)
  
  (require/typed racket/syntax
                 [format-id (→ Syntax String (U String Identifier) * Identifier)])
  ;(require racket/sequence) ;; in-syntax
  
  (require "sequences.rkt"
           #|"../low.rkt"|#) ;; my-in-syntax
  
  (define-type S-Id-List
    (U String
       Identifier
       (Listof String)
       (Listof Identifier)
       (Syntaxof (Listof Identifier))))
  
  (: format-ids (→ (U Syntax (→ (U String Identifier) * Syntax))
                   String
                   S-Id-List *
                   (Listof Identifier)))
  (define (format-ids lex-ctx format . vs)
    (let* ([seqs
            (map (λ ([v : S-Id-List])
                   (cond
                     [(string? v) (in-cycle (in-value v))]
                     [(identifier? v) (in-cycle (in-value v))]
                     [(list? v) (in-list v)]
                     [else (in-list (syntax->list v))]))
                 vs)]
           [justconstants (andmap (λ (x) (or (string? x) (identifier? x))) vs)]
           [seqlst (apply sequence-list seqs)])
      (for/list : (Listof Identifier)
        ([items seqlst]
         [bound-length (if justconstants
                           (in-value 'yes)
                           (in-cycle (in-value 'no)))])
        
        (apply format-id
               (if (procedure? lex-ctx) (apply lex-ctx items) lex-ctx)
               format
               items))))
  
  (: hyphen-ids (→ (U Syntax (→ (U String Identifier) * Syntax))
                   S-Id-List *
                   (Listof Identifier)))
  
  (define (hyphen-ids lex-ctx . vs)
    (apply format-ids
           lex-ctx
           (string-join (map (λ _ "~a") vs) "-")
           vs))
  
  (: format-temp-ids (→ String
                        S-Id-List *
                        (Listof Identifier)))
  
  (define (format-temp-ids format . vs)
    ;; Introduce the binding in a fresh scope.
    (apply format-ids (λ _ ((make-syntax-introducer) #'())) format vs)))

(module+ test
  (require ;(submod "..")
    ;"test-framework.rkt"
    (for-syntax racket/syntax
                (submod ".." ids-untyped)))
  
  (check-equal? (format-ids #'a "~a-~a" #'() #'())
                '())
  
  (check-equal? (map syntax->datum
                     (format-ids #'a "~a-~a" #'(x1 x2 x3) #'(a b c)))
                '(x1-a x2-b x3-c))
  
  ;; Since the presence of "Syntax" in the parameters list makes format-ids
  ;; require a chaperone contract instead of a flat contract, we can't run the
  ;; two tests below directly, we would need to require the untyped version of
  ;; this file, which causes a cycle in loading.
  
  (define-syntax (test1 stx)
    (syntax-case stx ()
      [(_ (let1 d1) x y)
       (begin
         (define/with-syntax (foo-x foo-y)
           (format-ids (λ (xy)
                         (if (string=? (symbol->string (syntax->datum xy))
                                       "b")
                             stx
                             #'()))
                       "foo-~a"
                       #'(x y)))
         #'(let1 d1 (let ((foo-b 2) (foo-c 'b)) (cons foo-x foo-y))))]))
  
  (check-equal? (test1 (let ((foo-b 1) (foo-c 'a))) b c)
                '(1 . b))
  
  (define-syntax (fubar stx)
    (define/with-syntax (v1 ...) #'(1 2 3))
    (define/with-syntax (v2 ...) #'('a 'b 'c))
    ;; the resulting ab and ab should be distinct identifiers:
    (define/with-syntax (id1 ...) (format-temp-ids "~a" #'(ab cd ab)))
    (define/with-syntax (id2 ...) (format-temp-ids "~a" #'(ab cd ab)))
    #'(let ([id1 v1] ...)
        (let ([id2 v2] ...)
          (list (cons id1 id2) ...))))
  
  (check-equal? (fubar) '((1 . a) (2 . b) (3 . c))))

#|
(define-template-metafunction (t/gen-temp stx)
  (syntax-parse stx
    [(_ . id:id)
     #:with (temp) (generate-temporaries #'(id))
     #'temp]
    [(_ id:id ...)
     (generate-temporaries #'(id ...))]))
|#

;; ==== syntax.rkt ====

(provide stx-assoc cdr-stx-assoc)
#|
(require/typed syntax/stx
               [stx-car (∀ (A B) (→ (Syntaxof (Pairof A B)) A))]
               [stx-cdr (∀ (A B) (→ (Syntaxof (Pairof A B)) B))])
|#
(: stx-car (∀ (A B) (→ (Syntaxof (Pairof A B)) A)))
(define (stx-car p) (car (syntax-e p)))

(: stx-cdr (∀ (A B) (→ (Syntaxof (Pairof A B)) B)))
(define (stx-cdr p) (cdr (syntax-e p)))

;(require/typed racket/base [(assoc assoc3) (∀ (a b) (→ Any (Listof (Pairof a b)) (U False (Pairof a b))))])
(require/typed racket/base
               [(assoc assoc3)
                (∀ (a b c) (case→ [→ Any
                                     (Listof (Pairof a b))
                                     (U False (Pairof a b))]
                                  [-> c
                                      (Listof (Pairof a b))
                                      (→ c a Boolean)
                                      (U False (Pairof a b))]))])

(: stx-assoc (∀ (T) (case→
                     (→ Identifier
                        (U (Syntaxof (Listof (Syntaxof (Pairof Identifier T))))
                           (Listof (Syntaxof (Pairof Identifier T))))
                        (U (Syntaxof (Pairof Identifier T)) #f))
                     (→ Identifier
                        (Listof (Pairof Identifier T))
                        (U (Pairof Identifier T) #f)))))
(define (stx-assoc id alist)
  (let* ([e-alist (if (syntax? alist)
                      (syntax->list alist)
                      alist)]
         [e-e-alist (cond
                      [(null? e-alist) '()]
                      [(syntax? (car e-alist))
                       (map (λ ([x : (Syntaxof (Pairof Identifier T))])
                              (cons (stx-car x) x))
                            e-alist)]
                      [else
                       (map (λ ([x : (Pairof Identifier T)])
                              (cons (car x) x))
                            e-alist)])]
         [result (assoc3 id e-e-alist free-identifier=?)])
    (if result (cdr result) #f)))

(: cdr-stx-assoc
   (∀ (T) (case→ (→ Identifier
                    (U (Syntaxof (Listof (Syntaxof (Pairof Identifier T))))
                       (Listof (Syntaxof (Pairof Identifier T)))
                       (Listof (Pairof Identifier T)))
                    (U T #f)))))
(define (cdr-stx-assoc id alist)
  (if (null? alist)
      #f
      ;; The typechecker is not precise enough, and the code below does not work
      ;; if we factorize it: (if (and (list? alist) (syntax? (car alist))) … …)
      (if (list? alist)
          (if (syntax? (car alist))
              (let ((res (stx-assoc id alist)))
                (if res (stx-cdr res) #f))
              (let ((res (stx-assoc id alist)))
                (if res (cdr res) #f)))
          (let ((res (stx-assoc id alist)))
            (if res (stx-cdr res) #f)))))

;; ==== generate-indices ====

(: generate-indices (∀ (T) (case→ (→ Integer (Syntax-Listof T) (Listof Integer))
                                  (→ (Syntax-Listof T) (Listof Nonnegative-Integer)))))

(provide generate-indices)

(define generate-indices
  (case-lambda
    [(start stx)
     (for/list ([v (my-in-syntax stx)]
                [i (in-naturals start)])
       i)]
    [(stx)
     (for/list ([v (my-in-syntax stx)]
                [i : Nonnegative-Integer (ann (in-naturals) (Sequenceof Nonnegative-Integer))])
       i)]))

;; ==== set.rkt ====

(provide set-map→set)
(: set-map→set (∀ (e b) (→ (Setof e) (→ e b) (Setof b))))
(define (set-map→set s f) (list->set (set-map s f)))

;; ==== type-inference-helpers.rkt ====

#|
;; This does not work, in the end.
(provide imap)
(define-syntax (imap stx)
  (syntax-parse stx
    [(_ lst:expr var:id (~optional (~literal →)) . body)
     #'(let ()
         (define #:∀ (T) (inlined-map [l : (Listof T)])
           (if (null? l)
               '()
               (cons (let ([var (car l)]) . body)
                     (inlined-map (cdr l)))))
         (inlined-map lst))]))
|#

;; ==== percent.rkt ====

(provide % define%)
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
    (pattern x:%pat
             #:with var-type #'x
             #:with (expanded ...) #'())))

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
|#

;; ==== end ====