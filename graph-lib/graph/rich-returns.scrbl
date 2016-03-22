#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Graphs with rich return types}

@section{The types in plain graphs}

Plain graphs use several types for their nodes:

@chunk[<graph-example>
       (define-graph g
         ;; Node types:
         [(a [field₁ : (List Foo Bar (Listof b) Baz Quux)]
             [field₂ : (Pairof c a)])]
         [(b [field₃] …)]
         [(c [field₇] …)]
         ;; Mappings: functions from external data to nodes (kinda)
         [ma (→ (Listof String) a)
          (a (list foo bar (map mb some-data) baz quux)
             (cons (mc more-data)
                   (ma other-data)))]
         [mb (→ Integer b)]
         [mc (→ … c)])]

Graph creationg is done by calling @racket[g] with the
parameters for the root mapping (@racket[ma] here).

@chunk[<graph-creation>
       (define g-instance (g '("some" "list" "of" "strings")))]

@chunk[<graph-use>
       g-instance.field₁.third.field₃.…]

Where third accesses the third element of the 
@racket[(List Foo Bar b Baz Quux)] list.

@chunk[|<graph types: placeholder>|
       (define-type a-placeholder Index)]

@chunk[|<graph typed: incomplete>|
       (define-tagged a-incomplete
         [field₁ : (List Foo Bar b-placeholder Baz Quux)]
         [field₂ : (Pairof c-placeholder a-placeholder)]
         …)]

@chunk[|<graph typed: promises>|
       (define-tagged a-promise
         [field₁ : (List Foo Bar b-promise Baz Quux)]
         [field₂ : (Pairof c-promise a-promise)]
         …)]

In @racket[a-promise], the structure is wrapped in a
promise, so that evaluating @racket[a-promise] does not
recursively evaluate the promises, which could cause
infinite recursion.

@section{Rich return types}

@chunk[<graph-example>
       (define-graph/rich-returns g
         ;; Node types (same):
         [(a [field₁ : (List Foo Bar (~> mb) Baz Quux)]
             [field₂ : (Pairof c a)])]
         [(b [field₃] …)]
         [(c [field₇] …)]
         ;; Mappings: functions from external data to data containing nodes
         [ma1 (→ (Listof String) a)
          (a (… (mb some-data) …)
             (cons (mc more-data)
                   (ma2 other-data)))]
         [ma2 (→ String Integer a) …]
         [mb (→ Integer (Listof b)) …]
         [mc (→ … c) …])]

This gets expanded to:

@chunk[<graph-example>
       (define-graph g1
         ;; Node types (same):
         [(a [field₁ : (List Foo Bar n-mb Baz Quux)]
             [field₂ : (Pairof n-mc n-ma)])]
         [(b [field₃] …)]
         [(c [field₇] …)]

         [(n-ma [val : a])]
         [(n-mb [val : (Listof b)])]
         [(n-mc [val : c])]
         ;; Mappings: functions from external data to nodes
         [m-n-ma (→ (Listof String) n-ma)
          (n-ma/incomplete
           (ma (… (m-n-mb some-data) …)
               (cons (m-n-mc more-data)
                     (m-n-ma2 other-data))))]
         [m-n-mb (→ Integer n-mb) …]
         [m-n-mc (→ … n-mc) …]
         [ma (→ arg1: (List Foo Bar n-mb/placeholder Baz Quux)
                arg2: (Pairof n-mc/placeholder
                              n-ma2/placeholder)
                a)
          (a/incomplete arg1 arg2)]
         [mb (→ ? b) …]
         [mc (→ ? c) …])]

@; TODO: (Pairof c a) => will actually be a n-mc and (U n-ma1 n-ma2).

Step 2: inlining

@chunk[<graph-example>
       (define-graph g2
         ;; Node types (same):
         [(a [field₁ : (List Foo Bar (Listof b) Baz Quux)]
             [field₂ : (Pairof c a)])]
         [(b [field₃] …)]
         [(c [field₇] …)]
         ;; Mappings: functions from g1 nodes to g2 nodes, made by inlining
         [ma (→ input: g1.a output: g2.a)
          ;; Input type:
          ;[field₁ : (List Foo Bar n-mb Baz Quux)]
          ;[field₂ : (Pairof n-mc n-ma)]
          (let ([_foo _bar _n-mb _baz _quux]
                [_n-mc . _n-ma])
               = input
            in
            (g2.a (list _foo _bar _n-mb.val _baz _quux)
                  (cons _n-mc.val _n-ma.val)))
          ]
         [mb (→ g1.b g2.b)]
         [mc (→ g1.c g2.c)])]

The complex bit:

@chunk[<graph-example>
       (define-graph/rich-returns ohlàlà
         ;; Node types:
         [(a [field₁ : (List Foo (~> mb))] …)]
         [(b [field₂ : xx] …)]
         [(c [field₃ : yy] …)]
         ;; Mappings: functions from external data to data containing nodes
         [ma (→ (Listof String) a)
          (a code… (mb some-args) code…)]
         [mb (→ Integer (List b (~> mc)))
          (list (b some-value)
                (mc some-args))]
         [mc (→ Symbol (Listof c))
          (map (λ (…)
                 (c some-value))
               …)])]

ohlàlà1: same as before (wrap each mapping with a
single-field node (the field is named "val").

ohlàlà2:

When inlining a:

@chunk[<inlining-a>
       [ma (→ input: ohlàlà1.a output: ohlàlà2.a)
          ;; Input type:
          ;[field₁ : (List Foo n-mb)]
          ;
          ; and n-mb has the single field:
          ;[val₂ : (List b n-mc)]
          ;
          ; and n-mc has the single field:
          ;[val₃ : (Listof c)]
          ;
          ;; Output type:
          ;
          ; (List Foo (List b (Listof c)))
          (let ([_foo _n-mb])
               = input
            in
            let ([_b _n-mc])
                = _n-mb.val₂
            in
            (g2.a (list _foo
                        (list _b
                              _n-mc.val₃))
                  ))
          ]
       ]

@chunk[<*>]