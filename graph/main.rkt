#lang typed/racket

(require "type-expander/type-expander.lp2.rkt")
(require "type-expander/multi-id.lp2.rkt")
(require "graph/variant.lp2.rkt")


(define-type from (List (Pairof Number Boolean)
                        (Listof (U Number (Pairof Number String)))))
(define-type to (List (Pairof String Boolean)
                      (Listof (U String (Pairof String String)))))

(: convert1 (→ from to))
(define (convert1 v)
  (match v [(list a b) (list (convert2 a) (convert3 b))]))

(: convert2 (→ (Pairof Number Boolean) (Pairof String Boolean)))
(define (convert2 v)
  (match v [(cons a b) (cons (convert4 a) (convert5 b))]))

(: convert3 (→ (Listof (U Number (Pairof Number String)))
               (Listof (U String (Pairof String String)))))
(define (convert3 v)
  (match v [(? list?) (map convert6 v)]))

(: convert4 (→ Number String))
(define (convert4 v)
  (match v [(? number?) (format "~a" v)]))

(: convert5 (→ Boolean Boolean))
(define (convert5 v)
  (match v [(? boolean?) v]))

(: convert6 (→ (U Number (Pairof Number String))
               (U String (Pairof String String))))
(define (convert6 v)
  (match v
    [(? number?) (format "~a" v)]
    [(? pair?) (cons (convert4 (car v)) (convert7 (cdr v)))]))

(: convert7 (→ String String))
(define (convert7 v)
  (match v [(? string?) v]))

(require typed/rackunit)
(check-equal? (convert1 '((123 . #t) (1 2 (3 . "b") 4 (5 . "x") 6)))
              '(("123" . #t) ("1" "2" ("3" . "b") "4" ("5" . "x") "6")))




#|
(define-type from (List (Pairof Number Boolean) (Listof Number)))
(define-type to (List (Pairof String Boolean) (Listof String)))

(: convert (case→ (→ from to)
                  (→ (Pairof (Listof Number) Null) (Pairof (Listof String) Null))
                  (→ (Pairof Number Boolean) (Pairof String Boolean))
                  (→ (Listof Number) (Listof String))
                  (→ Number String)
                  (→ Boolean Boolean)))
(define (convert v)
  (cond
    [(pair? v) (cons (convert (car v)) (convert (cdr v)))]
    [(null? v) v]
    [(number? v) (format "~a" v)]
    [(boolean? v) v]))
|#







;; Tests with incomplete / outer-incomplete type-expander.

(define-type-expander (outer-incomplete stx)
  (syntax-case stx ()
    [(_ n)
     #;(raise-syntax-error
        'incomplete
        (format "Type doesn't have an incomplete counterpart: ~a"
                (syntax->datum #'n))
        #'n)
     ;; Just for testing:
     #''error]))

(define-type C Boolean)

(define-type C/incomplete (Pairof 'C Boolean))

(define-type-expander (incomplete stx)
  (syntax-case stx ()
    [(_ n)
     (cond [(free-identifier=? #'n #'C) #'C/incomplete]
           [else #'(outer-incomplete n)])]))

(let ()
  (define-type-expander (outer-incomplete stx)
    (syntax-case stx () [(_ n) #'(incomplete n)]))
  (let ()
    (define-type A Number)
    (define-type B String)
    
    (define-type A/incomplete (Pairof 'A Number))
    (define-type B/incomplete (Pairof 'B String))
    
    (define-type-expander (incomplete stx)
      (syntax-case stx ()
        [(_ n)
         (cond [(free-identifier=? #'n #'A) #'A/incomplete]
               [(free-identifier=? #'n #'B) #'B/incomplete]
               [else
                #'(outer-incomplete n)])]))
    
    (define-type TA A)
    (define-type TAI (incomplete A))
    (displayln (ann '(A . 1) TAI))
    
    (define-type TC C)
    (define-type TCI (incomplete C))
    (displayln (ann #t TC))
    (displayln (ann '(C . #t) TCI))
    
    (let ()
      (define-type A Boolean)
      (define-type TA A)
      (define-type TAI (incomplete A))
      (displayln (ann 'error TAI))
      (void))))

(require (prefix-in tr: typed/racket))

;(define-type ma (tagged ma (fav String) (faa ma) (fab mb)))
;(define-type mb (tagged mb (fbv String) (fba ma)))

;(define-type ma (List (U ma Number) (U ma Number)) #:omit-define-syntaxes)
;(define-multi-id ma
;  #:match-expander (λ (stx) #'(list a b))
;  #:call (λ (stx) #'(list 1 (list 2 3))))

;(match (ann (ma) ma)
;  [(ma) #t])


#|
(module m typed/racket
  (provide ma)
  (require "type-expander/type-expander.lp2.rkt")
  (require "graph/variant.lp2.rkt")
  
  ;(let ()
  ;(define-tagged ma (fav String))
  ;(define-tagged ma (fav String) (faa ma) (fab mb))
  (define-tagged ma (fav String) (faa ma) (fab Number))
  ;(define-tagged mb (fbv String) (fba ma))
  (define-type ma/incomplete ma)
  ;(define-type mb/incomplete mb)
  (void);)
  )

(require 'm)
|#

#|
(require "graph/graph.rkt")

(define ma "boom")

(graph g
       [ma (fav String)
           (faa ma)
           (fab mb)]
       [mb (fbv String)
           (fba ma)])

(define mb "boom")
|#

#|
(require typed/rackunit)

;(require "graph/structure.lp2.rkt")
;(get ((make-struct-constructor a b c d) 1 "b" 'value-c 4) c)

(require "type-expander/type-expander.lp2.rkt")
(: w0 `(2 "abc" #,,(Pairof (U 'x 'y) (U 'y 'z)) #(1 "b" x) d))
(define w0 '(2 "abc" #,(x . z) #(1 "b" x) d))

(require (for-syntax racket/list))
(define-type-expander (Repeat stx)
  (syntax-case stx ()
    [(_ t n) #`(List #,@(map (λ (x) #'t)
                             (range (syntax->datum #'n))))]))

(: x (→ (Repeat Number 5)))
(define (x) (list 1 2 3 4 5))
(check-equal? (x) '(1 2 3 4 5))

(require "graph/structure.lp2.rkt")
(define-structure st2 [b String] [a Number])

(module* test typed/racket
  (require (submod "..")))

|#
