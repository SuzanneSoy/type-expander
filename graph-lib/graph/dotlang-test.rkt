#lang typed/racket

(module test "dotlang.rkt"
  (require typed/rackunit
           "../lib/low.rkt"
           "get.lp2.rkt"
           (submod "graph-test.rkt" test)
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

  (check-equal?: 'aa…bb (list 'get 'aa '… 'bb))
  (check-equal?: 'aa… (slen 3 "aa…"))
  
  (check-equal?: '… (slen 1 "…"))
  
  #|
  (check-equal?: '…aa.…bb..cc.d) ;; TODO: should cause error
  (check-equal?: '…aa….bb..cc.d) ;; TODO: should cause error
  |#)