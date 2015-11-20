#lang debug scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{The queue library}

@(table-of-contents)

@section{Introduction}

@section{Implementation}

@chunk[<fold-queues-signature>
       (fold-queues root-value
                    [(name [element (~literal :) Element-Type] Δ-queues enqueue)
                     (~literal :) result-type
                     . body]
                    ...)]

@chunk[<define-enqueue-type>
       (define/with-syntax enqueue/type
         #'(∀ (X) (case→ (→ 'name Element-Type X (values Index X))
                         ...)))]

@chunk[<define-Δ-queues-type>
       (define/with-syntax queues/type
         #'(List (Δ-Hash Element-Type Index) ...))]

@chunk[<fold-queue-multi-sets-immutable-tags>
       (define-syntax/parse <fold-queues-signature>
         <define-enqueue-type>
         <define-Δ-queues-type>
         #'(list (λ ([element : Element-Type]
                     [enqueue : enqueue/type]
                     [Δ-queues : queues/type])
                   : result-type
                   . body)
                 ...)
         #;#'(error "Not implemented yet"))]


@tc[Δ-Hash] is a type encapsulating both a hash, and a set of key-value pairs
added to the @tc[Δ-Hash] since its creation from a simple @tc[HashTable].

@chunk[<Δ-hash>
       (define-type (Δ-Hash A B)
         (Pairof (HashTable A B)
                 (Setof (Pairof A B))))
       
       (: empty-Δ-hash (∀ (K V) (→ (Δ-Hash K V))))
       (define (empty-Δ-hash)
         (cons ((inst hash K V)) ((inst set (Pairof K V)))))
       
       (: Δ-hash (∀ (K V) (→ (HashTable K V) (Δ-Hash K V))))
       (define (Δ-hash h)
         (cons h ((inst set (Pairof K V)))))
       
       (: Δ-hash-add (∀ (K V) (→ (Δ-Hash K V) K V
                                 (Δ-Hash K V))))
       (define (Δ-hash-add dh k v)
         (if (hash-has-key? (car dh) k)
             dh
             (cons (hash-set (car dh) k v)
                   (set-add (cdr dh) (cons k v)))))]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              racket/syntax
                              "../lib/low-untyped.rkt")
                  "../lib/low.rkt")
         
         (provide fold-queues)
         
         <Δ-hash>
         <fold-queue-multi-sets-immutable-tags>)]

@chunk[<module-test>
       (module* test typed/racket
         (require (submod "..")
                  typed/rackunit)
         
         ; TODO
         
         (require (submod ".." doc)))]

@chunk[<*>
       (begin
         <module-main>
         
         (require 'main)
         (provide (all-from-out 'main))
         
         <module-test>)]

