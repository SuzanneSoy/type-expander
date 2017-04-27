#lang racket/base

(require racket/require
         (subtract-in typed/racket type-expander)
         type-expander)

(provide (all-from-out typed/racket
                       type-expander))

(module reader syntax/module-reader
  type-expander/lang/main)