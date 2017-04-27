#lang typed/racket
(require type-expander
         (for-syntax racket/list)
         (for-syntax type-expander/expander
                     typed/rackunit
                     debug-scopes))

(debug-type-expander #t)
