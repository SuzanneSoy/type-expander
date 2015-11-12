#lang typed/racket/no-check

;; When creating the html document with scribble/lp2, it does not see the macros defined in low.rkt when including it with sugar/include.
;; Using a raw include/reader works.

;(require sugar/include)
;(include-without-lang-line "low.rkt")

;; TODO: file a bug report?
;; typed/racket/no-check does not require (for-syntax racket/base).
(require (for-syntax racket/base))

(include/reader "low.rkt" (λ (source-name in)
                              (port-count-lines! in)
                              (do ()
                                [(let-values ([(line column position)
                                               (port-next-location in)])
                                   (> line 1))]
                                (read-line in))
                              (read-syntax source-name in)))