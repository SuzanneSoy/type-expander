#lang scribble/lp2
@(require "../lib/doc.rkt")
@doc-lib-setup

@title[#:style manual-doc-style]{Implementation of structures}

@(table-of-contents)

@section{@racket[define-structure]}

Structures are represented using regular racket @tc[struct]s, see
@seclink["structures" #:doc "type-system.scrbl"]{the overview document}.
@;secref["structures" #:doc "type-system.scrbl"].

@chunk[<structure>
       (define-multi-id structure
         #:type-expander structure-type-expander
         #:match-expander structure-match-expander
         #:call (λ/syntax-parse (_ [field value] ...)
                  #'((make-structure-constructor field ...) value ...)))]

@chunk[<define-structure>
       (define-syntax (define-structure stx)
         (syntax-parse stx
           [(_ name [field type] ...)
            (define/with-syntax ([sorted-field sorted-type] ...)
              (sort-car-fields #'([field type] ...)))
            (define/with-syntax (pat ...) (generate-temporaries #'(field ...)))
            #'(define-multi-id name
                #:type-expand-once
                (structure [field type] ...)
                #:match-expander
                (λ (stx2)
                  (syntax-case stx2 ()
                    [(_ pat ...) #'(structure [field pat] ...)]))
                #:else
                (inst (make-structure-constructor field ...) type ...))]))]


@chunk[<test-define-structure>
       (define-structure st [a Number] [b String])
       (define-structure st2 [b String] [a Number])]

Test constructor:

@chunk[<test-define-structure>
       (check-equal:? (get (st 1 "b") b) : String "b")
       (check-equal:? (get (st2 "a" 2) b) : String "a")]

Test constructor, as id:

@chunk[<test-define-structure>
       (check-equal:? (get (cadr (map st '(1 2 3) '("x" "y" "z"))) b)
                      : String "y")
       (check-equal:? (get (cadr (map st2 '("d" "e" "f") '(1 2 3))) b)
                      : String "e")]

Test the type-expander:

@chunk[<test-define-structure>
       (check-equal? (get (ann (st2 "g" 123) st2) b) "g")]

Test the match-expander:

@chunk[<test-define-structure>
       (check-equal:? (match (st2 "h" 7) [(st x y) (cons x y)])
                      : (Pairof Number String)
                      '(7 . "h"))]

Test equality:

@chunk[<test-define-structure>
       (check-equal? (ann (st 1 "i") st) (st 1 "i"))
       (check-equal? (ann (st2 "j" 2) st2) (st2 "j" 2))
       (check-equal? (ann (st 1 "k") st) (st2 "k" 1))]

@section{Pre-declaring structs}

We wish to pre-declare all @tc[struct] types for various reasons:

@itemlist[
 @item{Anonymous, on-the-fly declaration (otherwise we need to be in a
  definition-context to be able to declare a @racket[struct]).}
 @item{If we use @code{(get-field s b)} in module @code{A}, and define a
  @racket[struct] type with a field @code{b} in module @code{B}, then the module
  @code{A} would have to require @code{B}, and we could easily run into cyclic
  dependencies.
  
  Moving the @racket[struct] definition to another place solves that problem.}]

In order to pre-declare the @tc[struct]s, we need to remember them across
compilations, for that we use @tc[remember-all] and @tc[get-remembered] defined
below in section @secref{remember}. We then need to make these identifiers
available in the correct syntax scope. The easiest way to do that, is to have a
private macro @tc[(declare-all-structs (name field ...) ...)] which does all the
required definitions, namely defining the struct, as well as @tc[make-struct],
@tc[get-field], and the match-expander eventually.

We do not wish to remember the type of each field, as they may be a non-exported
identifier from some module. It should not cause any problem when declaring the
parameter type @tc[(U g1 g2 …)] for a compiler pass, because by then, we should
have access to all the types we care about, and fill the rest with @tc[∀] types.

@chunk[<check-remember-fields>
       (define-for-syntax (check-remember-fields fields)
         (check-remember-all 'structure (sort-fields fields)))]

Since get-field is a macro, it should not care about the type of the field(s),
and the code it expands to should be a @tc[cond] which only tests the field part
of the structure.

@CHUNK[<declare-all-structs>
       (define-syntax/parse (declare-all-structs fields→stx-name-alist:id
                                                 (name field ...) ...)
         #'(begin
             <struct-declarations>
             
             (define-for-syntax fields→stx-name-alist
               (map (λ (x) (cons (map syntax->datum
                                      (syntax->list (stx-cdr x)))
                                 (stx-car x)))
                    (syntax->list #'((name field ...) ...))))))]

This macro should be called only once, and given as parameters the whole
remembered list of structs:

@CHUNK[<declare-all-structs>
       (define-syntax/parse (call-declare-all-structs fields→stx-name-alist:id)
         #`(declare-all-structs fields→stx-name-alist
                                #,@named-sorted-structures))
       
       (call-declare-all-structs fields→stx-name-alist)]

This list of structures associates their collection of fields with an arbitrary
name. The fields are sorted lexicographically and duplicate entries are removed,
so that @tc[(structure a b)] and @tc[(structure b a)] are equivalent, and only
one low-level @tc[struct] is generated for them.

@CHUNK[<named-sorted-structures>
       (define-for-syntax named-sorted-structures
         (for/list ([s (remove-duplicates (map (λ (s) (sort s symbol<?))
                                               (get-remembered 'structure)))]
                    [i (in-naturals)])
           `(,(string->symbol (format "struct-~a" i)) . ,s)))]

We will also need utility functions to sort the fields when querying this
associative list.

@chunk[<sort-car-fields>
       (define-for-syntax (sort-car-fields car-fields)
         (sort (syntax->list car-fields)
               symbol<?
               #:key (∘ syntax-e stx-car)))]

@chunk[<sort-fields>
       (define-for-syntax (sort-fields fields)
         (sort (syntax->list fields)
               symbol<?
               #:key syntax-e))]

@subsection{Type}

The struct declarations are rather standard. We use @tc[#:transparent], so that
@tc[equal?] compares instances memberwise.

@; TODO: write “field : Tfield”, it's cleaner.
@CHUNK[<struct-declarations>
       (struct (field ...) name ([field : field] ...) #:transparent)
       ...]

@section{Constructor}

We provide a macro which returns an anonymous @tc[structure] constructor. It can
be used to make @tc[structure] instances like this:

@chunk[<test-make-structure-constructor>
       (check-equal? (begin ((make-structure-constructor a b c) 1 "b" #t)
                            #t)
                     #t)]

To create such an instance, we use the underlying @tc[struct]'s constructor.
First, we need to check if the list of fields was already remembered, in which
case we return the associated @tc[struct] name. Otherwise, we trigger an error,
knowing that the list of fields has been remembered, so the next compilation
should succeed.

@CHUNK[<make-structure-constructor>
       (define-syntax/parse (make-structure-constructor field ...)
         (if (check-remember-fields #'(field ...))
             (let ()
               (define/with-syntax (sorted-field ...)
                 (sort-fields #'(field ...)))
               (define/with-syntax (TTemp ...)
                 (generate-temporaries #'(field ...)))
               #`(λ #:∀ (TTemp ...) ([field : TTemp] ...)
                   (#,(fields→stx-name #'(field ...)) sorted-field ...)))
             (remember-all-errors #'list stx #'(field ...))))]

To get the structure name from the list of fields, we need to sort
lexicographically the list of fields during lookup in
@tc[fields→stx-name-alist].
The fields in @tc[fields→stx-name-alist] are already sorted.

@chunk[<fields→stx-name>
       (define-for-syntax (fields→stx-name fields)
         (cdr (assoc (syntax->datum (datum->syntax #f (sort-fields fields)))
                     fields→stx-name-alist)))]

@subsection{Accessor}

@CHUNK[<get-field2>
       (define-syntax/parse (get v field:id)
         (define structs (filter (λ (s)
                                   (member (syntax->datum #'field) (car s)))
                                 fields→stx-name-alist))
         (define/with-syntax (name? ...)
           (map (λ (s) <get-predicate>) structs))
         (define/with-syntax (name-field ...)
           (map (λ (s) <get-field-accessor>) structs))
         #`(let ([v-cache v])
             (cond
               [(name? v-cache)
                (let ([accessor name-field])
                  (accessor v-cache))]; cover does not see the call otherwise?
               ...
               [else (typecheck-fail #,stx #:covered-id v-cache)])))]

@chunk[<get-predicate>
       (my-st-type-info-predicate (get-struct-info stx (cdr s)))]

@CHUNK[<get-field-accessor>
       (list-ref (my-st-type-info-accessors (get-struct-info stx (cdr s)))
                 (indexof (syntax->datum #'field) (reverse (car s))))]

@chunk[<test-get-field>
       (check-equal:?
        (get ((make-structure-constructor a b c d) 1 "b" 'value-c 4) c)
        : 'value-c
        'value-c)]

@subsection{Match-expander}

@chunk[<syntax-class-for-match>
       (begin-for-syntax
         (define-syntax-class match-field-or-field-pat
           (pattern [field:id pat ...])
           (pattern field:id #:with (pat ...) #'())))]

@chunk[<match-expander>
       <syntax-class-for-match>
       (define-for-syntax (structure-match-expander stx)
         (syntax-parse stx
           [(_ :match-field-or-field-pat ...)
            (if (check-remember-fields #'(field ...))
                (let ()
                  (define/with-syntax name (fields→stx-name #'(field ...)))
                  (define/with-syntax ([sorted-field sorted-pat ...] ...)
                    (sort-car-fields #'((field pat ...) ...)))
                  #'(name (and sorted-field sorted-pat ...) ...))
                <match-expander-remember-error>)]))]

If we just return @racket[(remember-all-errors list stx #'(field ...))] when a
recompilation is needed, then the identifier @tc[delayed-error-please-recompile]
becomes a variable bound by @tc[match], and may proceed without triggering any
error, if the body of the clause works without that part of the pattern (either
it does not use the variables defined within, or they were shadowing other
variables).

Therefore, we use that unbound identifier in a way for it to be read (to trigger
the error), but not bound to part of a pattern. Furthermore, in case the
sub-patterns in @tc[(pat ...)] contain themselves structs that have not yet been
remembered, we use them (without caring about what they match), so that they are
expanded, and get a chance to remember what they need for the next compilation,
instead of needing an extra recompilation.

@CHUNK[<match-expander-remember-error>
       #`(app #,(remember-all-errors #'list stx #'(field ...))
              (and pat ...) ...)]

@chunk[<test-match-expander>
       (let ([test-match 
              (λ ([val : Any])
                (match val
                  [(structure a b c y) (list a b c y)]
                  [(structure d
                              [a (? number?)]
                              [c (? symbol?) 'value-c]
                              [b bb (? string?)])
                   (list a bb c d)]
                  [else 'other]))])
         (check-equal? (test-match
                        ((make-structure-constructor a b c d) 1 "b" 'value-c 4))
                       '(1 "b" value-c 4))
         (check-equal? (test-match
                        ((make-structure-constructor a b c y) 1 2 3 4))
                       '(1 2 3 4))
         (check-equal? (test-match 'bad) 'other))]

@subsection{Anonymous type}

@subsection{Accessing information about racket's structs at compile-time}
@chunk[<my-st-type-info>
       (begin-for-syntax
         (struct my-st-type-info
           (type-descriptor
            constructor
            predicate
            accessors
            mutators
            super-type)
           #:transparent))]

@CHUNK[<struct-info>
       (define-for-syntax (get-struct-info stx s)
         (let* ([fail (λ () (raise-syntax-error 'get-struct-info
                                                "not a structure definition"
                                                stx
                                                s))]
                [v (if (identifier? s)
                       (syntax-local-value s fail)
                       (fail))]
                [i (if (not (struct-info? v)) (fail) (extract-struct-info v))])
           (apply my-st-type-info i)))]

@subsection{Type-expander}

@CHUNK[<type-expander>
       (define-for-syntax (structure-type-expander stx)
         (syntax-parse stx
           [(_ [field type] ...)
            (if (check-remember-fields #'(field ...))
                (let ()
                  (define/with-syntax ([sorted-field sorted-type] ...)
                    (sort-car-fields #'((field type) ...)))
                  (if (null? (syntax->list #'(sorted-type ...)))
                      (fields→stx-name #'(field ...))
                      #`(#,(fields→stx-name #'(field ...)) sorted-type ...)))
                (remember-all-errors #'U stx #'(field ...)))]))]

@chunk[<test-type-expander>
       (check-equal? (get (ann ((make-structure-constructor a b c) 1 "b" #t)
                               (structure [a Number] [c Boolean] [b String]))
                          b)
                     "b")]

@section[#:tag "remember"]{Closed-world assumption and global compilation}

In order to be able to access elements in the list as deep as they can be, we
need to know the length of the longest structure used in the whole program. 

Knowing what structures exist and what elements they contain can only help, so
we'll remember that instead.

The @tc[remember-all] for-syntax function below memorizes its arguments across
compilations, and adds them to the file “@code{remember.rkt}”:

@CHUNK[<remember-all>
       (require (for-syntax "remember.rkt"))
       
       (define-for-syntax (check-remember-all category value)
         (let ([datum-value (syntax->datum (datum->syntax #f value))])
           (if (not (member (cons category datum-value) all-remembered-list))
               (let ((file-name (build-path (this-expression-source-directory)
                                            "remember.rkt")))
                 ;; Add the missing field names to all-fields.rkt
                 (with-output-file [port file-name] #:exists 'append
                                   (writeln (cons category datum-value) port))
                 #f)
               #t)))
       
       (define-for-syntax (remember-all-errors id fallback stx-list)
         ;<remember-all-hard-error>
         #`(#,id #,(for/list ([cause `(,@(syntax->list stx-list) ,fallback)])
                     (syntax/loc cause delayed-error-please-recompile))))]

@CHUNK[<remember-all-hard-error>
       (raise-syntax-error
        (car (syntax->datum stx))
        (format "The fields ~a were added to ~a. Please recompile now."
                (string-join (map symbol->string missing) ", ")
                file-name)
        #f
        #f
        (filter (λ (f) (not (member (syntax->datum f) all-fields)))
                (syntax->list fields)))]

We can, during subsequent compilations, retrieve the list of already-memorized
fields for a given tag.

@CHUNK[<get-remembered>
       (define-for-syntax (get-remembered category)
         (cdr (or (assoc category all-remembered-alist) '(_))))]

If we start with an empty “@code{remember.rkt}” file, it will throw an error at
each call with a not-yet-remembered value. In order to avoid that, we use the
macro @tc[(delayed-error-please-recompile)], which expands to an undefined
identifier @code{please-recompile}. That error is caught later, and gives a
chance to more calls to @tc[remember-all] to be executed during macro-expansion.
We define @tc[delayed-error-please-recompile] in a submodule, to minimize the
chances that we could write a definition for that identifier.

@CHUNK[<delayed-error-please-recompile>
       (begin-for-syntax
         (module m-please-recompile typed/racket
           (define-syntax (delayed-error-please-recompile stx)
             #'please-recompile)
           (provide delayed-error-please-recompile))
         
         (require 'm-please-recompile))]

@section{Conclusion}

@chunk[<*>
       (begin
         (module main typed/racket
           (require (for-syntax racket
                                racket/syntax
                                syntax/parse
                                mzlib/etc
                                racket/struct-info
                                syntax/stx
                                racket/sequence
                                ;; in-syntax on older versions:
                                ;;;unstable/sequence
                                "../lib/low-untyped.rkt")
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    "../type-expander/multi-id.lp2.rkt")
           (provide define-structure
                    make-structure-constructor
                    get
                    structure)
           
           <remember-all>
           <get-remembered>
           <check-remember-fields>
           
           <named-sorted-structures>
           <sort-car-fields>
           <sort-fields>
           <declare-all-structs>
           <fields→stx-name>
           <make-structure-constructor>
           <delayed-error-please-recompile>
           
           <my-st-type-info>
           <struct-info>
           <get-field2>
           ;<get-field>
           
           <match-expander>
           <type-expander>
           
           <structure>
           <define-structure>)
         
         (require 'main)
         (provide (all-from-out 'main))
         
         (module* test typed/racket
           (require (submod "..")
                    "../lib/low.rkt"
                    "../type-expander/type-expander.lp2.rkt"
                    typed/rackunit)
           
           <test-make-structure-constructor>
           <test-get-field>
           <test-match-expander>
           <test-type-expander>
           <test-define-structure>
           
           (require (submod ".." doc))))]

@section{Optimizing access to fields}

We can represent the structuress as lists of key/value pairs. Then, if we know
the field @tc[b] can be at indices 3, 5 and 8 (for exeample), then we can access
it in 3, 5 or 8 steps, depending on the value's structure type, because we have
to traverse the list elements until we reach its value. In the worst case, the
access can be done in @${O(\mathit{max\_index\_for\_b})}, but on average it
should be less because of the fields with low indices.

We can also represent the structures using racket's structs, this is the method
chosen above. Then, to access the field @tc[b], we need to use the @tc[sᵢ-b]
field, where @tc[sᵢ] is the struct type of the value. To know that type, we need
to test the value's type against all the struct types, which costs
@${O(\left\vert{}S\right\vert)} then the access via @tc[sᵢ-b] can be done in
@${O(1)}.

A possible optimization would be to add an extra @tc[type-tag] field to all
structs, via a base struct @tc[(struct (T) Base ([type-tag : T]))]. Then, the
child structs would be defined so:

@chunk[<optimized-child-struct>
       (struct (T) Child-Struct Base ([g : String]))
       (define-type Child (Child-Struct Child-Tag))]

The tag can be constructed so that a value's struct type can be known in
@${O(\log{} \left\vert{}S\right\vert)}.
