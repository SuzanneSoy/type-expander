#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Purely functional queue library}

@(table-of-contents)

@section{Introduction}

We need a function behaving like @tc[fold], but which allows to add elements to
the list being processed. In other words, we want to apply a @tc[lambda] to all
elements inside a queue, with the possibility to return new elements that should
be added to the queue.

We will first define a general version in section @secref{sec:general}, where
most implementation details are parametrizable. We then use this as a building
block to define more specialized version, for example the section
@secref{sec:sets-add} defines a version where the queue is a set, and elements
can only be added to the queue, not removed (therefore the @tc[lambda] is
expected to return only the elements to be added, and not the ones already
present in the queue).

@section[#:tag "sec:general"]{General version}

Since both lists, sets and other structures could be used to represent the
queue, we don't specify them here, and instead take the @tc[dequeue] and
@tc[empty?] functions as a parameter.

We also will leave the choice of how the result value is constructed: the third
argument transforms the accumulator into the result value for an empty queue,
but for non-empty queues, we directly return the result of calling @tc[process]
on the first element, leaving to @tc[process] the responsibility to combine the
result for this element with the remaining ones, either by constructing the
overall result in the @tc[accumulator], or by using the value returned by
@tc[rec].

The former case is shown in section @secref{sec:tail-call-reverse-map}, where
each element's result is combined with the accumulator using @tc[cons], whereas
the latter case is shown in @secref{sec:map}, where the result for an element is
combined with @tc[cons] to what @tc[process-rest] will return.

The lambda @tc[process] takes four parameters: the @tc[element] to process, the
current accumulator, current queue, and a function to call recursively with the
new accumulator and queue, and which will return the final result once recursive
calls have exhausted the queue.

The new queue passed to the recursive function should contain both the new
pending elements to add to the queue, and the ones passed as argument (unless
some of those need to be deleted before being processed).

@chunk[<fold-queue>
       (: fold-queue (∀ (Queue Element Accumulator Result)
                        (→ Queue
                           Accumulator
                           (→ Accumulator Result)
                           (→ Queue (Values Element Queue))
                           (→ Queue Boolean)
                           (→ Element
                              Accumulator
                              Queue
                              (→ Queue Accumulator Result)
                              Result)
                           Result)))]

@chunk[<fold-queue>
       (define (fold-queue initial-queue accumulator last-result dequeue empty? process)
         (let process-rest ([queue initial-queue] [accumulator accumulator])
           (if (empty? queue)
               (last-result accumulator)
               (% element rest-queue = (dequeue queue)
                  (process element accumulator rest-queue process-rest)))))]

@section[#:tag "sec:map"]{@racket[map]}

Here is a version that behaves like @tc[map] on the queue, returning a list of
the result of @tc[process] on the queue's elements, in the order in which they
were dequeued.

Here, @tc[process] takes an element, the current @tc[accumulator], the remaining
unprocessed elements in @tc[rest-queue], and returns three values: the
@tc[result] of processing the @tc[element], the @tc[new-accumulator] (which will
be discarded once all elements have been processed), and the @tc[new-queue].

@chunk[<map-queue>
       (: map-queue (∀ (Queue Element Result Accumulator)
                       (→ Queue
                          Accumulator
                          (→ Queue (Values Element Queue))
                          (→ Queue Boolean)
                          (→ Element
                             Accumulator
                             Queue
                             (Values Result Queue Accumulator))
                          (Listof Result))))]

@chunk[<map-queue>
       (define (map-queue queue accumulator dequeue empty? process)
         (fold-queue queue
                     accumulator
                     (λ (_) : (Listof Result) '())
                     dequeue
                     empty?
                     <map-queue-process>))]

@chunk[<map-queue-process>
       (ann (λ (element accumulator rest-queue process-rest)
              (% result new-queue new-accumulator
                 = (process element accumulator rest-queue)
                 (cons result (process-rest new-queue new-accumulator))))
            (→ Element Accumulator Queue (→ Queue Accumulator (Listof Result))
               (Listof Result)))]

@section[#:tag "sec:tail-call-reverse-map"]{Tail-call @racket[map], with results
 in reverse order}

@chunk[<tail-call-reverse-map-queue>
       (: tail-call-reverse-map-queue
          (∀ (Queue Element Result Accumulator)
             (→ Queue
                Accumulator
                (→ Queue (Values Element Queue))
                (→ Queue Boolean)
                (→ Element
                   Accumulator
                   Queue
                   (Values Result Queue Accumulator))
                (Listof Result))))]

@chunk[<tail-call-reverse-map-queue>
       (define (tail-call-reverse-map-queue
                queue accumulator dequeue empty? process)
         (define-type RAccumulator (Pairof (Listof Result) Accumulator))
         (fold-queue queue
                     (cons (ann '() (Listof Result)) accumulator)
                     (inst car (Listof Result) Accumulator)
                     dequeue
                     empty?
                     <tail-call-reverse-map-queue-process>))]

@chunk[<tail-call-reverse-map-queue-process>
       (ann (λ (element accumulator rest-queue process-rest)
              (% result new-queue new-accumulator
                 = (process element (cdr accumulator) rest-queue)
                 (process-rest new-queue
                               (cons (cons result (car accumulator))
                                     new-accumulator))))
            (→ Element RAccumulator Queue (→ Queue RAccumulator (Listof Result))
               (Listof Result)))]

@section[#:tag "sec:sets-add"]{Variant using sets}

We define in this section a fold over queues represented using sets. This
version also disallows removing elements from the queue by making the union of
the existing queue and the new one returned by the @tc[process] function,
instead of using the latter to replace the former.

Moreover, this function does not build just a list of results, instead it builds
a dictionary with the processed elements as keys, and the corresponding results
as values.

@chunk[<fold-queue-sets>
       (: fold-queue-sets
          (∀ (Element Accumulator Result ResultAccumulator)
             (→ (Setof Element)
                Accumulator
                (→ Element Accumulator
                   (Values Result Accumulator (Setof Element)))
                ResultAccumulator
                (→ Element Result ResultAccumulator ResultAccumulator)
                (values ResultAccumulator Accumulator))))]

@chunk[<inst-fold-queue-sets>
       (inst fold-queue
             (Setof Element)
             Element
             (List (Setof Element) ResultAccumulator Accumulator)
             (List ResultAccumulator Accumulator))]

@chunk[<fold-queue-sets>
       (define (fold-queue-sets initial-queue accumulator process
                                combine-init combine-results)
         (apply values
                (<inst-fold-queue-sets>
                 initial-queue
                 (list ((inst set Element))
                       combine-init;((inst hash Element Result))
                       accumulator)
                 cdr
                 (λ ([s : (Setof Element)]) (values (set-first s) (set-rest s)))
                 set-empty?
                 <fold-queue-sets-process>)))]

@chunk[<fold-queue-sets-process>
       (λ (element accumulator rest-queue process-rest)
         (% (past-queue result-acc acc) = accumulator
            result new-acc more-elements = (process element acc)
            
            (process-rest
             (set-union rest-queue (set-subtract more-elements past-queue))
             (list (set-add past-queue element)
                   (combine-results element result result-acc)
                   #;(if (hash-has-key? result-hash element)
                         (error (string-append
                                 "Duplicate key in fold-queue-sets."
                                 "Are you using mutable elements?"))
                         (hash-set result-hash element result))
                   new-acc))))]

@subsection{Adding tags, using a mutable dictionary}

We build upon this version a new one which allows associating a custom tag for
each element. This tag allows symbolically to refer to an element which hasn't
been processed yet, and we return two dictionaries, one associating elements
with their tag and result, and the other associating tags with their element and
result.

Here is a first implementation using a mutable store of tags, and also makes it
unnecessary for the @tc[process] lambda to return the set of elements to add to
the queue (since all elements for which a tag is requested are implicitly added
to the queue).

@chunk[<fold-queue-sets-tags>
       (: fold-queue-sets-tags
          (∀ (Element Accumulator Tag Result)
             (→ (Setof Element)
                Accumulator
                (→ Element Accumulator (Values Tag Accumulator))
                (→ Element
                   Accumulator
                   (→ Element Accumulator (Values Tag Accumulator))
                   (values Result Accumulator))
                (Values (HashTable Element Result)
                        (HashTable Tag Result)
                        Accumulator))))]

@chunk[<inst-fold-queue-sets-tags>
       (inst fold-queue-sets
             (Pairof Element Tag)
             Accumulator
             Result
             (Pairof (HashTable Element Result) (HashTable Tag Result)))]

@chunk[<fold-queue-sets-tags>
       (define (fold-queue-sets-tags initial-queue accumulator make-tag process)
         (% all-tags = ((inst hash Element Tag))
            initial-tagged-queue new-accumulator = 
            (map+fold (λ ([e : Element] [acc : Accumulator])
                        (% tag new-acc = (make-tag e acc)
                           (values (cons e tag) new-acc)))
                      accumulator
                      (set->list initial-queue))
            
            (ht-element . ht-tag) last-accumulator =
            (<inst-fold-queue-sets-tags>
             (list->set initial-tagged-queue)
             new-accumulator
             (λ (e acc)
               (let ([new-tagged ((inst set (Pairof Element Tag)))])
                 <mutable-get-tag-for>
                 (% result new-acc = (process (car e) acc mget-tag-for)
                    (values result new-acc new-tagged))))
             (cons ((inst hash Element Result)) ((inst hash Tag Result)))
             (λ (e r racc)
               (cons (hash-set (car racc) (car e) r)
                     (hash-set (cdr racc) (cdr e) r))))
            
            (values ht-element ht-tag last-accumulator)))]

@chunk[<mutable-get-tag-for>
       (define (mget-tag-for [x : Element] [acc : Accumulator])
         (if (hash-has-key? all-tags x)
             (values (hash-ref all-tags x) acc)
             (% tag new-acc = (make-tag x acc)
                (set! new-tagged (set-add new-tagged (cons x tag)))
                (hash-set all-tags x tag)
                (values tag new-acc))))]

@subsection{Adding tags, using an immutable dictionary}

An alternative approach is to provide these tags using an immutable dictionary:
the @tc[get-tag] function now returns a tag and the new dictionary.

@chunk[<fold-queue-sets-immutable-tags>
       (: fold-queue-sets-immutable-tags
          (∀ (Element Accumulator Tag Result)
             (→ (Setof Element)
                Accumulator
                (→ Element Accumulator (Values Tag Accumulator))
                <fold-queue-sets-immutable-tags-process-type>
                (Values (HashTable Element Result)
                        (HashTable Tag Result)
                        Accumulator))))]

The @tc[process] lambda now takes an element, as well as a purely functional tag
provider, which uses an opaque database type @tc[X] to know for which elements
was a tag requested.

@chunk[<fold-queue-sets-immutable-tags-process-type>
       (∀ (X) (→ Element
                 Accumulator
                 X
                 (→ Element Accumulator X (Values Tag Accumulator X))
                 (Values Result Accumulator X)))]

@chunk[<inst-fold-queue-sets-immutable-tags>
       (inst fold-queue-sets
             (Pairof Element Tag)
             (Pairof (HashTable Element Tag) Accumulator)
             Result
             (Pairof (HashTable Element Result) (HashTable Tag Result)))]

@chunk[<fold-queue-sets-immutable-tags>
       (define (fold-queue-sets-immutable-tags initial-queue
                                               accumulator
                                               make-tag
                                               process)
         <immutable-get-tag-for>
         (% initial-tagged-queue new-accumulator = 
            (map+fold (λ ([e : Element] [acc : Accumulator])
                        (% tag new-acc = (make-tag e acc)
                           (values (cons e tag) new-acc)))
                      accumulator
                      (set->list initial-queue))
            
            (ht-element . ht-tag) (_ . result-acc) =
            (<inst-fold-queue-sets-immutable-tags>
             (list->set initial-tagged-queue)
             (cons ((inst hash Element Tag)) new-accumulator)
             (λ (e h+acc)
               (% (h . acc) = h+acc
                  empty-s = ((inst set (Pairof Element Tag)))
                  r new-acc (_ . s) = (process (car e)
                                               acc
                                               (cons h empty-s)
                                               get-tag-for)
                  new-h = (hash-set** h (set->list s))
                  (values r (cons new-h new-acc) s)))
             
             (cons ((inst hash Element Result)) ((inst hash Tag Result)))
             (λ (e r racc)
               (cons (hash-set (car racc) (car e) r)
                     (hash-set (cdr racc) (cdr e) r))))
            
            (values ht-element ht-tag result-acc)))]

@chunk[<immutable-get-tag-for>
       (: get-tag-for (→ Element
                         Accumulator
                         (Pairof (HashTable Element Tag)
                                 (Setof (Pairof Element Tag)))
                         (values Tag
                                 Accumulator 
                                 (Pairof (HashTable Element Tag)
                                         (Setof (Pairof Element Tag))))))
       (define% (get-tag-for x acc (h . s))
         (if (hash-has-key? h x)
             (values (hash-ref h x)
                     acc
                     (cons h s))
             (% tag new-acc = (make-tag x acc)
                (values tag
                        new-acc
                        (cons (hash-set h x tag) (set-add s `(,x . ,tag)))))))]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax syntax/parse
                                racket/syntax
                                "../lib/low-untyped.rkt")
                    "../lib/low.rkt"
                    racket/set)
           
           (provide fold-queue
                    map-queue
                    tail-call-reverse-map-queue
                    fold-queue-sets
                    fold-queue-sets-tags
                    fold-queue-sets-immutable-tags)
           
           <fold-queue>
           <map-queue>
           <tail-call-reverse-map-queue>
           <fold-queue-sets>
           <fold-queue-sets-tags>
           <fold-queue-sets-immutable-tags>)
         
         (require typed/racket)
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    typed/rackunit)
           
           ((inst fold-queue-sets-immutable-tags
                  Integer
                  Void
                  String
                  (List 'a Integer String))
            (set 10 11 12)
            (void)
            (λ (e acc) (values (format "{~a}" e) acc))
            (λ (e acc x get-tag)
              (let*-values ([(t1 acc1 x1) (get-tag (if (even? e)
                                                       (floor (/ e 2))
                                                       (+ (* 3 e) 1))
                                                   acc
                                                   x)]
                            [(t2 acc2 x2) (get-tag 127 acc1 x1)])
                (values (list 'a e t1) acc2 x2))))
           
           (require (submod ".." doc))))]