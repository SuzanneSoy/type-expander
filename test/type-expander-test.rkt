#lang typed/racket

(require type-expander
         typed/rackunit
         (for-syntax type-expander/expander
                     racket/list
                     version/utils
                     rackunit
                     syntax/parse))

; Tests for expand-type
(begin
  ;; Test harness:
  (begin
    (define-syntax (test-expander stx)
      (syntax-parse stx
        [(_ type expanded-type)
         (let ([actual (syntax->datum (expand-type #'type))]
               [expected (syntax->datum #'expanded-type)])
           (unless (equal? actual expected)
             (raise-syntax-error
              'test-expander
              (format "test-expander failed: expected ~a, got ~a"
                      expected
                      actual)
              stx
              #'type))
           #`(check-equal? '#,actual
                           '#,expected))])))

  ; Simple identity expander test, with a different case when used as a
  ; simple identifier.

  (begin
    (define-type-expander (id stx)
      (syntax-case stx ()
        [(_ t) #'t]
        [x #'(∀ (A) (→ A A))]))

    (test-expander (id Number) Number)
    (test-expander id (∀ (A) (→ A A))))

  (begin
    (define-type-expander (double stx)
      (syntax-case stx ()
        [(_ t) #'(id (Pairof (id t) t))]))

    (test-expander (∀ (A) (→ A (id (double (id A)))))
                   (∀ (A) (→ A (Pairof A A))))

    (test-expander (→ Any Boolean : (double (id A)))
                   (→ Any Boolean : (Pairof A A))))

  ;; Curry expander arguments:
  (begin
    (define-type-expander (CPairof stx)
      (syntax-case stx ()
        [(_ a) #'(curry Pairof a)]
        [(_ a b) #'(Pairof a b)]))

    (test-expander (CPairof Number String)
                   (Pairof Number String))

    (test-expander ((CPairof Number) String)
                   (Pairof Number String))

    (check-equal? (ann (ann '(1 . "b") (CPairof Number String))
                       (Pairof Number String))
                  '(1 . "b"))

    (check-equal? (ann (ann '(1 . "c") ((CPairof Number) String))
                       (Pairof Number String))
                  '(1 . "c")))

  ;; Shadowing with ∀ variables:
  (begin
    (test-expander (∀ (id) (→ id))
                   (∀ (id) (→ id)))
    (test-expander (∀ (id2) (→ id))
                   (∀ (id2) (→ (∀ (A) (→ A A))))))

  (begin
    (define-type-expander (Repeat stx)
      (syntax-case stx ()
        [(_ t n) #`(List #,@(map (λ (x) #'t)
                                 (range (syntax->datum #'n))))]))

    (test-expander (Repeat Number 5)
                   (List Number Number Number Number Number)))

  (begin
    (: count-five-more (→ Number (Repeat Number 5)))
    (define (count-five-more x)
      (list (+ x 1) (+ x 2) (+ x 3) (+ x 4) (+ x 5)))

    (check-equal? (count-five-more 3)
                  '(4 5 6 7 8))
    (check-equal? (ann (count-five-more 15) (Repeat Number 5))
                  '(16 17 18 19 20)))

  ;; Shadowing with Rec variables:

  (begin
    (: repeat-shadow (→ Number (Rec Repeat (U Null (List Number Repeat)))))
    (define (repeat-shadow n)
      (if (= n 0)
          '()
          (list n (repeat-shadow (sub1 n)))))
    (check-equal? (repeat-shadow 5)
                  '(5 (4 (3 (2 (1 ()))))))
    (test-expander (→ Number (Rec Repeat (U Null (List Number Repeat))))
                   (→ Number (Rec Repeat (U Null (List Number Repeat))))))

  ;; Shadowing with Let:

  (begin
    (let ()
      (define-type-expander (exp stx)
        #'(List 1 2 3))

      (define-type e String)
      (: x (List e (Let ([e exp]) e)))
      (define x (list "e1" (list 1 2 3)))
      (check-equal? x '("e1" (1 2 3)))
      (test-expander (List e (Let ([e exp]) e))
                     (List e (List 1 2 3)))

      (: y (List e))
      (define y (list "e2"))
      (check-equal? y '("e2"))
      (test-expander (List e)
                     (List e))
      (void)))

  ;; Let, Λ and ∀
  (begin
    (let ()
      (define-type-expander Str1 (λ (_) #'String))

      (test-expander (Let ([Number Str1]) Number)
                     String)
      (test-expander (Let ([Number (Λ stx #'String)]) (Number))
                     String)
      (test-expander (Let ([Number (Λ stx #'Str1)]) (Number))
                     String)
      (test-expander (Let ([Number (Λ stx #'String)]) Number)
                     String)

      (test-expander ((∀ (A) (Pairof A A)) Number)
                     (Pairof Number Number))
      (test-expander (Let ([String (∀ (A) (Pairof A A))])
                       (String Number))
                     (Pairof Number Number))

      (test-expander (Let ([Poly-Repeat
                            (Λ (_ n)
                               #`(∀ (A)
                                    (List #,@(map (λ (_) #'A)
                                                  (range (syntax-e #'n))))))]
                           [Number String])
                       ((Poly-Repeat 5) Number))
                     (List String String String String String))

      (test-expander (Let ([Poly-Repeat
                            (Λ (_ n)
                               #`(∀ (A)
                                    (List #,@(map (λ (_) #'A)
                                                  ;; like above, but also works
                                                  ;; without the syntax-e here:
                                                  (range n)))))]
                           [Number String])
                       ((Poly-Repeat 5) Number))
                     (List String String String String String))

      (ann '(1 . "b") ((Let ([Foo String])
                         (∀ (ty)
                            (Pairof ty Foo)))
                       Integer))

      (test-expander ((∀ (A1)
                         (Let ()
                           (Let ()
                             (Let ()
                               (Let ()
                                 A1)))))
                      Number)
                     Number)

      (void)))

  ;; Let*, Letrec
  (let ()
    (test-expander
     (Letrec ([Poly-Repeat
               (Λ (_ n)
                  (if (= 0 n)
                      #'(∀ (A) Null)
                      #`(∀ (A)
                           (Pairof A
                                   ((Poly-Repeat #,(sub1 n)) A)))))]
              [Number String])
       ((Poly-Repeat 5) Number))
     (Pairof String
             (Pairof String
                     (Pairof String
                             (Pairof String
                                     (Pairof String Null))))))

    #;(test-expander (Let* ([String Number]
                            [Number String])
                       (List Number String))
                     (List Number Number))
    (void)))

;; Test ":"
(begin
  (: c0 `(2 "abc" #,,(Pairof (U 'x 'y) (U 'y 'z)) #(1 "b" x) d))
  (define c0 '(2 "abc" #,(x . z) #(1 "b" x) d))

  (let ()
    (define-type-expander (Repeat stx)
      (syntax-case stx ()
        [(_ t n) #`(List #,@(map (λ (x) #'t)
                                 (range (syntax->datum #'n))))]))

    (: x (→ (Repeat Number 5)))
    (define (x) (list 1 2 3 4 5))
    (check-equal? (x) '(1 2 3 4 5))))

;; Test define-type
(let ()
  (define-type-expander (Repeat stx)
    (syntax-case stx ()
      [(_ t n) #`(List #,@(map (λ (x) #'t)
                               (range (syntax->datum #'n))))]))

  (define-type R5 (Repeat Number 5))
  (check-equal? (ann '(1 2 3 4 5) R5) '(1 2 3 4 5)))

;; Test define
(begin
  (define d0
    : `(2 "abc" #,,(Pairof (U 'x 'y) (U 'y 'z)) #(1 "b" x) d)
    '(2 "abc" #,(x . z) #(1 "b" x) d))
  (check-equal? (ann d0 (List 2
                              "abc"
                              (List 'unsyntax
                                    (Pairof (U 'x 'y) (U 'y 'z)))
                              (Vector 1 "b" 'x) 'd))
                '(2 "abc" (unsyntax (x . z)) #(1 "b" x) d))

  (: d1 (→ Number (→ Number Number)))
  (define ((d1 [x : Number]) [y : Number]) : Number (+ x y))
  (check-equal? (ann ((d1 2) 3) Number) 5)

  (: d2 (→ Number (→ Number Number)))
  (define ((d2 [x : Number]) [y : Number]) (+ x y))
  (check-equal? (ann ((d2 3) 4) Number) 7)

  (define #:∀ (T) ((d3 [x : T]) [y : T]) : (Pairof T T) (cons x y))
  (check-equal? (ann ((d3 'x) 'y) (Pairof Symbol Symbol)) '(x . y)))

;; Test lambda
(begin
  (check-equal? ((ann (lambda ([x : Number]) : Number (* x 2))
                      (→ Number Number))
                 3)
                6)
  (check-equal? ((ann (λ ([x : Number]) : Number (* x 2))
                      (→ Number Number))
                 3)
                6)
  (check-equal? ((λ x x) 1 2 3) '(1 2 3))
  (check-equal? ((λ #:∀ (A) [x : A ...*] : (Listof A) x) 1 2 3) '(1 2 3))
  (check-equal? ((λ [x : Number ...*] : (Listof Number) x) 1 2 3) '(1 2 3))
  (check-not-exn (λ ()
                   (ann (λ #:∀ (A) [x : A ...*] : (Listof A) x)
                        (∀ (A) (→ A * (Listof A))))))
  (check-not-exn (λ ()
                   (ann (λ #:∀ (A) [x : A *] : (Listof A) x)
                        (∀ (A) (→ A * (Listof A))))))
  (check-not-exn (λ ()
                   (ann (λ #:∀ (A ...) ([l : (List A ... A)]) : (List A ... A)
                          l)
                        (∀ (A ...) (→ (List A ... A) (List A ... A))))))
  (check-not-exn (λ ()
                   (ann (λ #:∀ (A ...) [l : (List A ... A) *]
                          : (Listof (List A ... A))
                          l)
                        (∀ (A ...) (→ (List A ... A) *
                                      (Listof (List A ... A)))))))
  (check-not-exn (λ ()
                   (ann (λ #:∀ (A ...) [l : (List A ... A) ...*]
                          : (Listof (List A ... A))
                          l)
                        (∀ (A ...) (→ (List A ... A) *
                                      (Listof (List A ... A))))))))

;; Test struct
(begin
  (struct s0 ())
  (struct s1 ([x : Number]))
  (struct s2 ([x : Number] [y : Number]))
  (struct s3 ([x : Number] [y : Number]) #:transparent)
  (struct s4 () #:transparent)
  (struct (A B) s5 ([x : A] [y : B]) #:transparent)
  (struct (A B) s6 () #:transparent)
  (struct s7 s2 ([z : String]) #:transparent)
  (struct (A) s8 s3 ([z : A]) #:transparent)
  (struct (A B C) s9 s5 ([z : C]) #:transparent)
  (struct (A B C) s10 s2 ([z : C]) #:transparent)
  (struct (A B C) s11 s5 ([z : C]))

  (check (λ (a b) (not (equal? a b))) (s0) (s0))
  (check-equal? (s1-x (s1 123)) 123)
  (check-equal? (s2-x (s2 2 3)) 2)
  (check-equal? (s2-y (s2 2 3)) 3)
  (check-equal? (s3-x (s3 4 5)) 4)
  (check-equal? (s3-y (s3 4 5)) 5)
  (check-equal? (s4) (s4))
  (check-equal? (s5-x (s5 6 7)) 6)
  (check-equal? (s5-y (s5 6 7)) 7)
  (check-equal? (s5 6 7) (s5 6 7))
  (check-equal? ((inst s5 Number String) 6 "g") (s5 6 "g"))
  (check-equal? (s6) (s6))
  (check-equal? ((inst s6 Number String)) (s6))

  ;(check-equal? (s7-x (s7 -1 -2 "c") -1))
  ;(check-equal? (s7-y (s7 -1 -2 "c") -2))
  (check-equal? (s7-z (s7 -1 -2 "c")) "c")
  (check-equal? (s2-x (s7 -1 -2 "c")) -1)
  (check-equal? (s2-y (s7 -1 -2 "c")) -2)
  (check-not-equal? (s7 -1 -2 "c") (s7 -1 -2 "c"))
  (check-not-exn (λ () (ann (s7 -1 -2 "c") s2)))
  (check-true (s2? (s7 -1 -2 "c")))

  ;(check-equal? (s8-x (s8 -1 -2 "c") -1))
  ;(check-equal? (s8-y (s8 -1 -2 "c") -2))
  (check-equal? (s8-z (s8 -1 -2 "c")) "c")
  (check-equal? (s3-x (s8 -1 -2 "c")) -1)
  (check-equal? (s3-y (s8 -1 -2 "c")) -2)
  (check-equal? (s8 -1 -2 "c") (s8 -1 -2 "c"))
  (check-equal? ((inst s8 String) -1 -2 "c") (s8 -1 -2 "c"))
  (check-not-exn (λ () (ann ((inst s8 String) -1 -2 "c") s3)))
  (check-true (s3? ((inst s8 String) -1 -2 "c")))

  ;(check-equal? (s9-x (s9 8 9 10)) 8)
  ;(check-equal? (s9-y (s9 8 9 10)) 9)
  (check-equal? (s9-z (s9 8 9 10)) 10)
  (check-equal? (s5-x (s9 8 9 10)) 8)
  (check-equal? (s5-y (s9 8 9 10)) 9)
  (check-equal? (s9 8 9 10) (s9 8 9 10))
  ;; Bug https://github.com/racket/typed-racket/issues/451
  ;(check-not-exn (λ () (ann ((inst s9 Number Symbol String) 8 'i "j")
  ;                          (Struct s5))))
  (check-not-exn (λ () (ann ((inst s9 Number Symbol String) 8 'i "j")
                            (Struct (s5 Number Symbol)))))
  (check-not-exn (λ () (ann ((inst s9 Number Symbol String) 8 'i "j")
                            (s5 Number Symbol))))
  (check-not-exn (λ () (ann ((inst s9 Number Symbol String) 8 'i "j")
                            (s5 Any Any))))
  (check-true (s5? ((inst s9 Number Symbol String) -1 'i "j")))
  (check-not-equal? (s10 11 12 13) (s10 11 12 13))
  (check-not-equal? (s11 14 15 16) (s11 14 15 16)))

;; Test define-struct/exec
(begin
  (define-struct/exec se0 ()
    ;[(λ (self v) (cons self v)) : (∀ (A) (→ se0 A (Pairof se0 A)))])
    [(λ (self v) (cons self v)) : (→ se0 Any (Pairof se0 Any))])
  (define-struct/exec se1 ([x : Number])
    ;[(λ (self v) (cons self v)) : (∀ (A) (→ se0 A (Pairof se0 A)))])
    [(λ (self v) (cons self v)) : (→ se1 Any (Pairof se1 Any))])
  (define-struct/exec se2 ([x : Number] [y : Number])
    [(λ (self v) (cons self v)) : (→ se2 Any (Pairof se2 Any))])
  (define-struct/exec (se3 se2) ([z : String])
    [(λ (self v w) (list self v w))
     ;: (∀ (A B) (→ se3 A B (List se2 A B)))])
     : (→ se3 Any Any (List se2 Any Any))])
  (define-struct/exec (se4 se2) ([z : String])
    [(λ (self v w) (list self v w))
     ;: (∀ (A B) (→ se4 A B (List se2 A B)))])
     : (→ se4 Any (→ Number Number) (List se2 Any (→ Number Number)))])

  (check (λ (a b) (not (equal? a b))) (se0) (se0))
  (check-equal? (cdr ((se0) 'a)) 'a)
  (check-not-exn (λ () (ann (car ((se0) 'a)) se0)))
  (check-true (se0? (car ((se0) 'a))))

  (check (λ (a b) (not (equal? a b))) (se1 123) (se1 123))
  (check-equal? (se1-x (se1 123)) 123)
  (check-equal? (se1-x (car ((se1 123) 'b))) 123)
  (check-equal? (cdr ((se1 123) 'b)) 'b)
  (check-not-exn (λ () (ann (car ((se1 123) 'b)) se1)))
  (check-true (se1? (car ((se1 123) 'b))))

  (check (λ (a b) (not (equal? a b))) (se2 2 3) (se2 2 3))
  (check-equal? (se2-x (se2 2 3)) 2)
  (check-equal? (se2-y (se2 2 3)) 3)
  (check-equal? (se2-x (car ((se2 2 3) 'c))) 2)
  (check-equal? (se2-y (car ((se2 2 3) 'c))) 3)
  (check-equal? (cdr ((se2 2 3) 'c)) 'c)
  (check-not-exn (λ () (ann (car ((se2 2 3) 'c)) se2)))
  (check-true (se2? (car ((se2 2 3) 'c))))

  (check (λ (a b) (not (equal? a b))) (se3 4 5 "f") (se3 4 5 "f"))
  (check-equal? (se2-x (se3 4 5 "f")) 4)
  (check-equal? (se2-y (se3 4 5 "f")) 5)
  (check-equal? (se3-z (se3 4 5 "f")) "f")
  (check-equal? (se2-x (car ((se3 4 5 "f") 'd 'e))) 4)
  (check-equal? (se2-y (car ((se3 4 5 "f") 'd 'e))) 5)
  (check-equal? (let ([ret : Any (car ((se3 4 5 "f") 'd 'e))])
                  (if (se3? ret)
                      (se3-z ret)
                      "wrong type!"))
                "f")
  (check-equal? (cadr ((se3 4 5 "f") 'd 'e)) 'd)
  (check-equal? (caddr ((se3 4 5 "f") 'd 'e)) 'e)
  (check-equal? ((caddr ((se4 4 5 "f") 'd (λ ([x : Number]) (* x 2)))) 12)
                24)
  (check-not-exn (λ () (ann (car ((se3 4 5 "f") 'd 'e)) se2)))
  (check-true (se2? (car ((se3 4 5 "f") 'd 'e))))
  (check-true (se3? (car ((se3 4 5 "f") 'd 'e)))))

;; Test ann
(let ()
  (define-type-expander (Repeat stx)
    (syntax-case stx ()
      [(_ t n) #`(List #,@(map (λ (x) #'t)
                               (range (syntax->datum #'n))))]))
  (check-equal? (ann (ann '(1 2 3)
                          (Repeat Number 3))
                     (List Number Number Number))
                '(1 2 3)))

;; Test inst
(let ()
  (define-type-expander (Repeat stx)
    (syntax-case stx ()
      [(_ t n) #`(List #,@(map (λ (x) #'t)
                               (range (syntax->datum #'n))))]))

  (: f (∀ (A B C D) (→ (Pairof A B) (Pairof C D) (List A C B D))))
  (define (f x y) (list (car x) (car y) (cdr x) (cdr y)))

  (check-equal? ((inst f
                       (Repeat Number 3)
                       (Repeat String 2)
                       (Repeat 'x 1)
                       (Repeat undefined-type 0))
                 '((1 2 3) . ("a" "b"))
                 '((x) . ()))
                '((1 2 3) (x) ("a" "b") ())))

;; Test let
(begin
  (check-equal? (ann (let loop-id ([x 1])
                       (if (equal? x 2)
                           x
                           (loop-id 2)))
                     Any)
                2)
  (check-equal? (let () 'x) 'x)
  (check-equal? (ann (let #:∀ (T) ([a : T 3]
                                   [b : (Pairof T T) '(5 . 7)])
                       (cons a b))
                     (Pairof Number (Pairof Number Number)))
                '(3 5 . 7)))

;; Test let*
(let ()
  (define-type-expander (Repeat stx)
    (syntax-case stx ()
      [(_ t n) #`(List #,@(map (λ (x) #'t)
                               (range (syntax->datum #'n))))]))

  (check-equal? (let* ([x* : (Repeat Number 3) '(1 2 3)]
                       [y* : (Repeat Number 3) x*])
                  y*)
                '(1 2 3)))

;; Test let-values
(let ()
  (define-type-expander (Repeat stx)
    (syntax-case stx ()
      [(_ t n) #`(List #,@(map (λ (x) #'t)
                               (range (syntax->datum #'n))))]))

  (check-equal? (ann (let-values
                         ([([x : (Repeat Number 3)])
                           (list 1 2 3)])
                       (cdr x))
                     (List Number Number))
                '(2 3))

  (check-equal? (ann (let-values
                         ([([x : (Repeat Number 3)] [y : Number])
                           (values (list 1 2 3) 4)])
                       (cons y x))
                     (Pairof Number (List Number Number Number)))
                '(4 . (1 2 3)))

  (check-equal? (ann (let-values
                         ([(x y)
                           (values (list 1 2 3) 4)])
                       (cons y x))
                     (Pairof Number (List Number Number Number)))
                '(4 . (1 2 3))))

;; Test make-predicate
(let ()
  (define-type-expander (Repeat stx)
    (syntax-case stx ()
      [(_ t n) #`(List #,@(map (λ (x) #'t)
                               (range (syntax->datum #'n))))]))
  (check-equal? ((make-predicate (Repeat Number 3)) '(1 2 3)) #t)
  (check-equal? ((make-predicate (Repeat Number 3)) '(1 "b" 3)) #f))

;; row-inst
(let-syntax ([when-row-inst-is-defined
              (λ (stx)
                (syntax-case stx ()
                  [(_ body)
                   (if (or (identifier-binding #'row-inst)
                           (version<=? "6.7" (version)))
                       #'body
                       #'(void))]))])
  (when-row-inst-is-defined
   (let ()
     ;; Example taken from the docs
     (: id (All (r #:row)
                (-> (Class #:row-var r) (Class #:row-var r))))
     (define (id cls) cls)
     (define result ((row-inst id (Row (field [x Integer])))
                     (class object% (super-new) (field [x : Integer 0]))))
     (ann result (Class (field (x Integer))))
     (void))))

;; Tests written while debugging
(begin
  (let ()
    (define-type-expander flob
      (λ (stx) #'(All (abc) (List abc))))

    (ann '(a) ((flob) Symbol))

    (ann '(42) ((Let ([flob (Λ (_ arg)
                               #'((∀ (abc)
                                     (List abc)) arg))])
                  flob)
                Integer))

    (ann '(1 2 3) ((∀ (abc)
                      (Listof abc))
                   Integer))

    (ann '(1 2 3) ((Let ()
                     (∀ (abc)
                        (Listof abc)))
                   Integer))
    (void))

  (let ()
    (test-expander
     ((Let () (∀ (A1) (Let () A1))) Number)
     Number))

  (let ()
    (ann '(1 2 3) (Listof Integer))

    (define-type (poly1 ty)
      (Listof ty))

    (ann '(1 2 3) (poly1 Integer))

    (define-type (poly2 ty)
      (Listof ty))

    (ann '(1 2 3) (poly2 Integer))

    (ann '(1 2 3) ((∀ (ty)
                      (Listof ty))
                   Integer))

    (ann '(1 2 3) ((Let ()
                     (∀ (ty)
                        (Listof ty)))
                   Integer))

    (void))

  (define-for-syntax (test-use/def stx def expected-use expected-def)
    (let ([actual-use (syntax->datum
                       (apply-type-expander (datum->syntax stx 'Quux)
                                            (datum->syntax stx 'Quux)))]
          [actual-def (syntax->datum
                       (apply-type-expander def def))])
      (check-equal? actual-use expected-use)
      (check-equal? actual-def expected-def)))

  (let ()
    (define-type-expander (Foo stx) #'Integer)
    (define-type-expander (Bar stx) #'Integer)
    (define-type-expander (Quux stx) #'Integer)

    (define-type mytype1
      (Let ([Foo (Λ (_ T)
                    (test-use/def #'T #'Quux 'Integer 'Integer)
                    #'T)])
        (Foo Quux)))

    (define-type mytype23
      (Let ([Quux String])
        (Let ([Foo (Λ (_ T)
                      (test-use/def #'T #'Quux 'String 'String)
                      #'T)])
          (Foo (∀ (A)
                  (Let ([Bar (Λ (_ T)
                                (test-use/def #'T #'Quux 'String 'String)
                                #'T)])
                    (Bar (Listof A))))))))

    (define-type mytype45
      (Let ([Foo (Λ (_ T)
                    (test-use/def #'T #'Quux 'String 'Integer)
                    #'T)])
        (Let ([Quux String])
          (Foo (∀ (A)
                  (Let ([Bar (Λ (_ T)
                                (test-use/def #'T #'Quux 'String 'String)
                                #'T)])
                    (Bar (Listof A))))))))

    (define-type mytype67
      (Let ([Foo (Λ (_ T)
                    (test-use/def #'T #'Quux 'Integer 'Integer)
                    #'T)])
        (Foo (Let ([Quux String])
               (∀ (A)
                  (Let ([Bar (Λ (_ T)
                                (test-use/def #'T #'Quux 'String 'String)
                                #'T)])
                    (Bar (Listof A))))))))

    (define-type mytype89
      (Let ([Foo (Λ (_ T)
                    (test-use/def #'T #'Quux 'Integer 'Integer)
                    #'T)])
        (Foo (∀ (A)
                (Let ([Quux String])
                  (Let ([Bar (Λ (_ T)
                                (test-use/def #'T #'Quux 'String 'String)
                                #'T)])
                    (Bar (Listof A))))))))

    (void))

  (let ()
    (test-expander ((Let ([F (Λ (_ T) #'T)]) F) String)
                   String)
    (test-expander ((Let ([F (Λ (_ T) #'(List T))]) F) String)
                   (List String)))

  ;; Please don't ever do that in practice :) !
  (let ()
    (test-expander (Let ([Loot Number])
                     ((Let ([Loot Let]) Loot) ([AAA Number])
                                              AAA))
                   Number)
    (test-expander (Let ([Loot Number])
                     ((Let ([Loot Let]) Loot) ([Let Loot])
                                              Let))
                   Number)
    (test-expander (Let ([Loot Number])
                     ((Let ([Loot Let]) Loot) ([Loot String])
                                              Loot))
                   String)))

;; more tests
(begin
  (test-expander ((∀ (A) ((∀ (A) ((∀ (A) ((∀ (A) A) A)) A)) A)) Number)
                 Number)
  (test-expander (Let ([A Number])
                   (Let ([A A])
                     (Let ([A A])
                       (Let ([A A])
                         A))))
                 Number)
  (test-expander (Let* ([A Number]
                        [A A]
                        [A A]
                        [A A]
                        [A A])
                   A)
                 Number)
  (test-expander (Let* ([A Number]
                        [A (List A)]
                        [A (Pairof A A)]
                        [A (Vector A)]
                        [A (Set A)])
                   A)
                 (Set (Vector (Pairof (List Number) (List Number)))))

  ;; Adjusted from http://www.cs.utah.edu/~mflatt/scope-sets/
  ;;   #%28part._.Macro_.Definitions_in_a_.Recursive_.Scope%29
  (test-expander (Letrec ([Identity (Λ (_ misc-id)
                                       #'(∀ (X)
                                            (Let ([misc-id String])
                                              X)))])
                   ((Identity X) Number))
                 Number)
  (test-expander (Letrec ([Identity (Λ (_ misc-id)
                                       #'(∀ (misc-id)
                                            (Let ([X String])
                                              misc-id)))])
                   ((Identity X) Number))
                 Number)
  (test-expander (Letrec ([GetY (Λ (_ misc-id)
                                       (datum->syntax #'misc-id 'Y))])
                   (Let ([Y Number]) (GetY X)))
                 Number)
  (test-expander (Letrec ([GetY (Λ (_ misc-id)
                                       (datum->syntax #'misc-id 'Y))])
                   ((∀ (Y) (GetY X)) Number))
                 Number))

;; Tests for Syntax
(let ()
  (test-expander #'a (Syntaxof 'a))
  (test-expander #'(a) (Syntaxof (List (Syntaxof 'a))))
  (test-expander #'(a . b) (Syntaxof (Pairof (Syntaxof 'a) (Syntaxof 'b))))
  (test-expander #'(a . (b))
                 (Syntaxof (Pairof (Syntaxof 'a)
                                   (Syntaxof (List (Syntaxof 'b))))))
  (test-expander #'(a b) (Syntaxof (List (Syntaxof 'a) (Syntaxof 'b))))
  (test-expander #'(a b . c)
                 (Syntaxof (List* (Syntaxof 'a) (Syntaxof 'b) (Syntaxof 'c))))
  (test-expander #'(a b . (c))
                 (Syntaxof (List* (Syntaxof 'a)
                                  (Syntaxof 'b)
                                  (Syntaxof (List (Syntaxof 'c)))))))

;; Small typo
(let ()
  (test-expander ((No-Expand List) 'a 'b) (List 'a 'b)))
