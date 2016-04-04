#lang scribble/manual

@(require scribble-enhanced/manual-form)

@(require (for-label typed/racket/base
                     "graph.lp2.rkt"))

@title{Low-level graph macro}


@defform[(graph …)
         #:result graph-id
         #:contracts ([old-type (syntax-for type)]
                      [from (and/c identifier? (syntax-for type))]
                      [to (syntax-for type)])]{
 …}

@defform[(graph-id #:roots [node args] …)
         #:result (List (Vectorof node/promise-type) …)
         #:contracts ([args (Listof (List arg-type …))])]{
 Create a graph instance, starting from the given root
 arguments. Each element of the returned list contains a
 vector with all the graph roots for that node type, in the
 same order as their arguments were given. If there are some
 duplicates in the lists of arguments, the same node will be
 returned for both.}