#lang scribble/manual

@title{Type expander: Implementation}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

This library is implemented using literate programming. The implementation
details are presented in the following sections. The user documentation is in
the @other-doc['(lib "type-expander/scribblings/type-expander.scrbl")] document.

@(table-of-contents)
@include-section[(submod "../type-expander.hl.rkt" doc)]
@include-section[(submod "../more-expanders.hl.rkt" doc)]
