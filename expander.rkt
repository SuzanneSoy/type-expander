#lang racket
(require (submod "type-expander.hl.rkt" expander)
         (for-template (submod "type-expander.hl.rkt" main))
         syntax/parse)
(provide prop:type-expander
         expand-type
         apply-type-expander
         type
         stx-type/c
         type-expand!
         colon)
