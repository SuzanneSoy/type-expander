#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide check-equal?-classes
           check-equal?-classes:)
  
  (require "typed-untyped.rkt")
  (require-typed/untyped "syntax-parse.rkt"
                         "sequence.rkt")
  
  (require (for-syntax syntax/parse
                       syntax/parse/experimental/template
                       racket/syntax
                       (submod "aliases.rkt" untyped)
                       (submod "syntax-parse.rkt" untyped)
                       (submod "repeat-stx.rkt" untyped))
           typed/rackunit)
  
  (: check-equal?-classes (∀ (A ...) (→ (Pairof String (Listof A)) ... Void)))
  (define (check-equal?-classes . classes)
    (for* ([(head tail) (in-split* classes)])
      (let ([this-class (sequence-ref tail 0)]
            [different-classes (in-sequences head (sequence-tail tail 1))])
        (for ([val (cdr this-class)])
          (for ([other-val (cdr this-class)])
            #;(displayln (format "Test ~a ∈ ~a = ~a ∈ ~a …"
                                 val
                                 this-class
                                 other-val
                                 this-class))
            (check-equal? val other-val
                          (format "Test ~a ∈ ~a = ~a ∈ ~a failed."
                                  val
                                  this-class
                                  other-val
                                  this-class)))
          (for ([different-class different-classes])
            (for ([different-val (cdr different-class)])
              #;(displayln (format "Test ~a ∈ ~a != ~a ∈ ~a ∈ ~a …"
                                   val
                                   this-class
                                   different-val
                                   different-class
                                   (sequence->list different-classes)))
              (check-not-equal? val different-val
                                (format "Test ~a ∈ ~a != ~a ∈ ~a ∈ ~a failed."
                                        val
                                        this-class
                                        different-val
                                        different-class
                                        (sequence->list
                                         different-classes)))))))))
  
  (define-syntax/parse (check-equal?-classes:
                        (~seq [(~maybe #:name name:expr)
                               (~maybe (~lit :) c-type)
                               (~seq val (~maybe (~lit :) v-type)) …])
                        …)
    (define/with-syntax ([a-val …] …)
      (template ([(?? (ann val v-type) val) …] …)))
    (define/with-syntax ([aa-val …] …)
      (let ()
        ;; TODO: this is ugly, repeat-stx should handle missing stuff instead.
        (define/with-syntax (xx-c-type …) (template ((?? (c-type) ()) …)))
        (syntax-parse (repeat-stx (xx-c-type …) ([val …] …))
          [([((~optional c-type-rep)) …] …)
           (template ([(?? name "") (?? (ann a-val c-type-rep) a-val) …] …))])))
    (template
     (check-equal?-classes (list aa-val …) …))))