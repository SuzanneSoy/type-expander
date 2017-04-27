#lang racket/base

(require racket/require
         (subtract-in typed/racket/base type-expander)
         type-expander)

(provide (all-from-out typed/racket/base
                       type-expander))

(module reader syntax/module-reader
  type-expander/lang/main)