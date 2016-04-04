#lang typed/racket

(require typed/rackunit)

;; tagged
(begin
  ;; Base struct for all tagged values
  (struct (B) tagged ([value : B]))
  
  ;; (define-tagged a T)
  ;; Use tag-a just for match and (get …)
  (struct (B) tag-a tagged ()) ;; "remember" this one
  ;; (define-tagged #:uninterned a T)
  (struct (B) tag-a-1 tag-a ()) ;; do not "rembmer" the #:uninterned ones
  ;; (define-tagged #:uninterned a T)
  (struct (B) tag-a-2 tag-a ())
  ;; (define-tagged #:uninterned a T)
  (struct (B) tag-a-3 tag-a ())
  ;; (define-tagged b T)
  (struct (B) tag-b tagged ())
  
  ;; instanciation:
  (tag-a "ice")
  
  (define-syntax-rule (define-pred name? tag?)
    (define-match-expander name?
      (λ (stx)
        (syntax-case stx ()
          [(_ v)
           #'(and (? tag?) (app tagged-value v))]))))
  
  (define-pred tagged-a-1? tag-a-1?)
  (define-pred tagged-a-2? tag-a-2?)
  (define-pred tagged-a? tag-a?)
  (define-pred tagged-b? tag-b?)
  
  (check-true (tag-a? (tag-a "water")))
  (check-true (tag-a-1? (tag-a-1 "fire")))
  (check-true (tag-a-1? (tag-a-1 "air")))
  (check-false (tag-a-1? (tag-a-2 "earth")))
  (check-false (tag-a-1? (tag-a "salt")))
  (check-false (tag-b? (tag-a "mercury")))
  (check-false (tag-b? (tag-a-1 "alchemy")))
  
  (λ ([x : (U (tag-a-1 String)
              (tag-a-2 Number)
              (tag-b Symbol))])
    : (U Number String)
    ;; Match should expand to this:
    (match x
      [(tagged-a? v) v]
      [(tagged-b? v) (symbol->string v)]))
  
  (λ ([x : (U (tag-a-1 String)
              (tag-a-2 String)
              (tag-a String)
              (tag-b Symbol))])
    : String
    ;; Match should expand to this
    (match x
      [(tagged-a-1? v) (string-append v "-1")]
      [(tagged-a-2? v) (string-append v "-2")]
      [(tagged-a? v) (string-append v "-base")]
      [(tagged-b? v) (symbol->string v)])))

;; struct
(begin
  ;; The structs seem to work well the way they are currently defined (using
  ;; "remember" to know all the structs with a given field).
  
  ;; (define-struct [x : Number] [y : String])
  (struct (X Y) structure+x+y ([x : X] [y : Y])) ;; "remember"
  (define-type structure+x=number+y=string (structure+x+y Number String))
  
  ;; (define-struct [x : Number] [y : String] [z : Symbol])
  (struct (X Y Z) structure+x+y+z ([x : X] [y : Y] [z : Z])) ;; "remember"
  (define-type structure+x=number+y=string+z=symbol
    (structure+x+y+z Number String Symbol))
  
  ;; (define-struct [z : Symbol])
  (struct (Z) structure+z ([z : Z])) ;; "remember"
  (define-type structure+z=symbol (structure+z Symbol))
  
  ;; (define-struct [x : Symbol])
  (struct (X) structure+x ([x : X])) ;; "remember"
  (define-type structure+x=symbol (structure+x Symbol))
  
  ;; (has-get [x Number])
  (define-type (has-get+x X) (U (structure+x X)
                                (structure+x+y X Any)
                                (structure+x+y+z X Any Any)))
  (define-type has-get+x=number (has-get+x Number))
  
  ;; (has-get [y Number])
  (define-type (has-get+y Y) (U (structure+x+y Any Y)
                                (structure+x+y+z Any Y Any)))
  (define-type has-get+y=number (has-get+y Number))
  
  ;; (has-get [z Number])
  (define-type (has-get+z Z) (U (structure+z Z)
                                (structure+x+y+z Any Any Z)))
  (define-type has-get+z=number (has-get+z Number))
  
  ;; (get v x)
  (define get-x
    (λ #:∀ (X) ([s : (has-get+x X)]) : X
      (cond
        [(structure+x? s) (structure+x-x s)]
        [(structure+x+y? s) (structure+x+y-x s)]
        [(structure+x+y+z? s) (structure+x+y+z-x s)])))
  
  ;; (get v y)
  (define get-y
    (λ #:∀ (Y) ([s : (has-get+y Y)]) : Y
      (cond
        [(structure+x+y? s) (structure+x+y-y s)]
        [(structure+x+y+z? s) (structure+x+y+z-y s)])))
  
  ;; (get v z)
  (define get-z
    (λ #:∀ (Z) ([s : (has-get+z Z)]) : Z
      (cond
        [(structure+z? s) (structure+z-z s)]
        [(structure+x+y+z? s) (structure+x+y+z-z s)]))))

;; graph
(begin
  ;; define-graph
  (define-type g-test (tag-a-1 (structure+x+y String g-test)))
  (define-type g (tag-a-2 (structure+x+y+z String g Number)))
  ;; pass:
  (λ ([root-node : (U g g-test)])
    (ann (match root-node
           [(tagged-a-1? s) (string-length (get-x s))]
           [(tagged-a-2? s) (get-z s)])
         Number)
    (cons (get-x (tagged-value root-node))
          (get-y (tagged-value root-node)))))
