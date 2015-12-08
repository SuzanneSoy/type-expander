#lang debug scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{The queue library}

@(table-of-contents)

@section{Introduction}

@section{Implementation}

@chunk[<fold-queues-signature>
       (fold-queues root-value
                    [(name [element (~literal :) Element-Type]
                           [Δ-queues (~literal :) Δ-Queues-Type-Name]
                           enqueue)
                     (~literal :) Result-Type
                     . body]
                    …
                    (~parse (root-name . _) #'(name …)))]

@chunk[<enqueue-type>
       (case→ (→ 'name
                 Element-Type
                 Δ-Queues-Type-Name
                 (values Index
                         Δ-Queues-Type-Name))
              …)]

@chunk[<define-queues-type___>
       (define/with-syntax queues-type
         #'(List (Δ-Hash Element-Type Index) …))]

@chunk[<define-ids>
       (define-temp-ids "~a/process-element" (name …))
       (define-temp-ids "~a/Δ-results-add" (name …))]

@chunk[<process-element-type>
       (∀ (Δ-Queues-Type-Name)
          (→ Element-Type
             Δ-Queues-Type-Name
             <enqueue-type>
             (values Result-Type
                     Δ-Queues-Type-Name)))]

@chunk[<fold-queue-multi-sets-immutable-tags>
       (define-syntax/parse <fold-queues-signature>
         ;<define-queues-type>
         <define-ids>
         #|((λ (x) (pretty-write (syntax->datum x)) x)|#
         #'(let ()
             (begin
               (: name/process-element <process-element-type>)
               (define (name/process-element element Δ-queues enqueue) . body))
             …
             <Δ-hash2-definitions>
             <Δ-results-definitions>
             <process-queues>)#|)|#)]

@subsection{Representation of the queues}

@;Each queue will be represented as a hash table associating

In order to have the indices in the right order, without having to sort the
result, the queues have to be simple lists.

However, the @tc[enqueue] function needs to detect duplicate elements, so it
needs the @tc[(HashTable Element-Type Index)].

We shall use both representation for the queues, adding a third integer value
which tracks the length of the list (i.e. the first unallocated result index):

@chunk[<Δ-hash2-definitions>
       <Δ-hash2-type>
       <Δ-hash2-empty>
       <Δ-hash2-enqueue>
       <Δ-hash2-dequeue>]

@chunk[<Δ-hash2-queue-type>
       (List (HashTable Element-Type Index)
             (Listof Element-Type) ; Reversed stack
             (Listof Element-Type) ; Stack
             Index)]

@chunk[<Δ-hash2-type>
       (define-type Δ-hash2-type (List <Δ-hash2-queue-type> …))]

@subsubsection{Notes on parallelism}

For parallelism, we can later change the signature of @tc[fold-queues], and say
we return a @tc[(Vectorof (Vectorof Result-Type))], with one vector for each
core. The tags will then be a pair of integers (or use modulo, and interleave
the queues). We should, for forward planning, make the index type and the result
database type opaque, and use an accessor with signature
@tc[(→ Opaque-Database Opaque-Index Result-Type)].

@subsubsection{Adding elements to the queues}

@chunk[<Δ-hash2-enqueue>
       (: Δ-hash2-enqueue (case→ (→ 'name
                                    Element-Type
                                    Δ-hash2-type
                                    (values Index
                                            Δ-hash2-type))
                                 …))
       (define (Δ-hash2-enqueue selector elt qs)
         (match-let ([(list name/queue …) qs])
           (cond [(eq? selector 'name)
                  (if (hash-has-key? (car name/queue) elt)
                      (values (hash-ref (car name/queue) elt)
                              qs)
                      (match-let ([(list h rs s i) name/queue])
                        (let* ([new-h (hash-set h elt i)]
                               [new-s (cons elt s)]
                               [new-i (+ i 1)]
                               [new-i-index (if (index? new-i)
                                                new-i
                                                (error "Too many elements"))]
                               [name/queue (list new-h rs new-s new-i-index)])
                          (values i
                                  (list name/queue …)))))]
                 …)))]

@subsubsection{Popping elements from the queues}

@chunk[<Δ-hash2-dequeue>
       (: Δ-hash2-dequeue (case→ (→ <Δ-hash2-queue-type>
                                    (values Element-Type
                                            <Δ-hash2-queue-type>))
                                 …))
       (define (Δ-hash2-dequeue q)
         (match q
           [(list h (cons e rest-rs) s i)
            (values e
                    (list h rest-rs s (assert (- i 1) index?)))]
           [(list h '() s i)
            (Δ-hash2-dequeue (list h (reverse s) '() i))]))]

@subsubsection{Constructor for the queues}

@chunk[<Δ-hash2-empty>
       (define Δ-hash2-empty
         (list (list ((inst hash Element-Type Index)) '() '() 0)
               …))]

@subsection{Result lists}

We accumulate results in lists, one per queue. Since the elements are processed
in the order in which they are added, the result lists contain them in reverse
order: the element at position @${i} in a list is associated with the index
@${n-i} in the hash table, where @${n} is the number of elements processed so
far for that queue.

The datatype is defined as follows:

@chunk[<Δ-results-definitions>
       <Δ-results-type>
       <Δ-results-add>
       <Δ-results-empty>
       <Δ-results-to-vectors>]

@chunk[<Δ-results-type>
       (define-type Δ-results-type (List (Listof Result-Type) …))]

We have a constructor for empty lists of results:

@chunk[<Δ-results-empty>
       (define Δ-results-empty (list (ann '() (Listof Result-Type)) …))]

And an operation to add a new result, for each queue name:

@chunk[<Δ-results-add>
       (begin
         (: name/Δ-results-add (→ Δ-results-type Result-Type Δ-results-type))
         (define (name/Δ-results-add Δ-results value)
           (match-let ([(list name/queue …) Δ-results])
             (let ([name/queue (cons value name/queue)]) ;; to shadow name/queue
               (list name/queue …)))))
       …]

Finally, we will need to return a list of vectors, with each element having its
position in the vector equal to the index associated to it in the hash table:

@chunk[<Δ-results-to-vectors>
       (: Δ-results-to-vectors (→ Δ-results-type
                                  (List (Vectorof Result-Type) …)))
       (define (Δ-results-to-vectors Δ-results)
         (match-let ([(list name/queue …)
                      (ann Δ-results (List (Listof Result-Type) …))])
           (list (vector->immutable-vector
                  (ann (list->vector (reverse name/queue))
                       (Vectorof Result-Type)))
                 …)))]

@subsection{Processing the queues}

@chunk[<define-ids>
       (define-temp-ids "~a/queue" (name …))]

@chunk[<process-queues>
       (define (process-queues [queues : Δ-hash2-type]
                               [results : Δ-results-type])
         : (List (Vectorof Result-Type) …)
         (match-let ([(list name/queue …) queues])
           (cond [(or (not (empty? (cadr name/queue)))
                      (not (empty? (caddr name/queue))))
                  <process-queue>]
                 …
                 [else (Δ-results-to-vectors results)])))
       
       (% index Δ-hash = (Δ-hash2-enqueue 'root-name root-value Δ-hash2-empty)
          (process-queues Δ-hash Δ-results-empty))]

@chunk[<process-queue>
       (% e name/queue = (Δ-hash2-dequeue name/queue) ;; to hide name/queue
          result new-Δ-queues = (name/process-element e
                                                      (list name/queue …)
                                                      Δ-hash2-enqueue)
          (process-queues new-Δ-queues (name/Δ-results-add results result)))]

@subsection{Δ-Hash}

@tc[Δ-Hash] is a type encapsulating both a hash, and a set of key-value pairs
added to the @tc[Δ-Hash] since its creation from a simple @tc[HashTable].

@chunk[<Δ-hash>
       (module Δ-hash typed/racket
         (require "../lib/low.rkt")
         (define-type (Δ-Hash A B)
           (Pairof (HashTable A B)
                   (Setof (Pairof A B))))
         
         (: empty-Δ-hash (∀ (K V) (→ (Δ-Hash K V))))
         (define (empty-Δ-hash)
           (cons ((inst hash K V)) ((inst set (Pairof K V)))))
         
         (: Δ-hash (∀ (K V) (→ (HashTable K V) (Δ-Hash K V))))
         (define (Δ-hash h)
           (cons h ((inst set (Pairof K V)))))
         
         (: Δ-hash-add (∀ (K V Acc) (→ (Δ-Hash K V)
                                       K
                                       Acc
                                       (→ K Acc (values V Acc))
                                       (values (Δ-Hash K V)
                                               Acc))))
         (define (Δ-hash-add Δ-hash k acc make-v)
           (if (hash-has-key? (car Δ-hash) k)
               (values Δ-hash acc)
               (% v new-acc = (make-v k acc)
                  (values (cons (hash-set (car Δ-hash) k v)
                                (set-add (cdr Δ-hash) (cons k v)))
                          new-acc))))
         
         (: Δ-hash-get-Δ (∀ (K V) (→ (Δ-Hash K V) (Setof (Pairof K V)))))
         (define (Δ-hash-get-Δ Δ-hash) (cdr Δ-hash)))]

@section{@racket{cond-let}}

@CHUNK[<cond-let>
       (define-syntax (cond-let stx)
         (syntax-parse stx
           [(_)
            #'(typecheck-fail #,stx)]
           [(_ #:let bindings:expr clause …)
            #'(let bindings (cond-let clause …))]
           [(_ [condition:expr (~seq #:else-let binding …) … . body] clause …)
            #'(if condition
                  (begin . body)
                  (let (binding … …)
                    (cond-let clause …)))]))]

@section{Conclusion}

@chunk[<module-main>
       (module main typed/racket
         (require (for-syntax syntax/parse
                              racket/syntax
                              racket/pretty; DEBUG
                              "../lib/low-untyped.rkt")
                  "../lib/low.rkt")
         
         (provide fold-queues)
         
         <cond-let>
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

