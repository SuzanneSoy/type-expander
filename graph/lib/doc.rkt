#lang racket

;; Math
(require slideshow/pict)
(provide (all-from-out slideshow/pict))
(require "doc/math.rkt")
(provide (all-from-out "doc/math.rkt"))
; @setup-math is returned in @doc-lib-setup.


(require scriblib/render-cond)

;(require "low-untyped.rkt")
;(#lang reader "scribble-custom/lp2.rkt" #:lang typed/racket)

;; http://lists.racket-lang.org/users/archive/2015-January/065752.html
;; http://bugs.racket-lang.org/query/?cmd=view%20audit-trail&pr=14068
;;                                                  &database=default
(require (for-label (only-meta-in 0 typed/racket)))
(provide (for-label (all-from-out typed/racket)))


;; ==== hybrid footnotes/margin-note ====
(provide note)
(require (only-in scriblib/footnote [note footnote])
         (only-in scribble/base margin-note)
         (only-in scribble/core nested-flow style))

(define (note . args)
  (cond-element
   [html (element (style "refpara" '())
                  (list (element (style "refcolumn" '())
                                 (list (element (style "refcontent" '())
                                                (list args))))))]
   [else (apply footnote args)]))

;; ==== ====

(require (for-syntax mzlib/etc))
(define-syntax (doc-lib-setup stx)
  ;(display (build-path (this-expression-source-directory) (this-expression-file-name)))
  #'setup-math) ;; NOTE: setup-math must be returned, not just called!

(provide doc-lib-setup)

;(require (only-in scribble/manual code))
;(define-syntax-rule (tc . args)
;  (code #:lang "typed/racket" . args))
;(provide tc)

(require (only-in scribble/private/lp chunk CHUNK))
(provide chunk CHUNK)






;; Copied from /usr/local/racket-6.2.900.6/share/pkgs/scribble-lib/scribble/private/lp.rkt

(require (for-syntax racket/base syntax/boundmap)
         scribble/scheme scribble/decode scribble/manual (except-in scribble/struct table))

(begin-for-syntax
  ;; maps chunk identifiers to a counter, so we can distinguish multiple uses
  ;; of the same name
  (define chunk-numbers (make-free-identifier-mapping))
  (define (get-chunk-number id)
    (free-identifier-mapping-get chunk-numbers id (lambda () #f)))
  (define (inc-chunk-number id)
    (free-identifier-mapping-put! chunk-numbers id (+ 1 (free-identifier-mapping-get chunk-numbers id))))
  (define (init-chunk-number id)
    (free-identifier-mapping-put! chunk-numbers id 2)))

(define-syntax-rule (make-chunk chunk-id racketblock)
  (define-syntax (chunk-id stx)
    (syntax-case stx ()
      [(_ name expr (... ...))
       ;; no need for more error checking, using chunk for the code will do that
       (identifier? #'name)
       (let* ([n (get-chunk-number (syntax-local-introduce #'name))]
              [str (symbol->string (syntax-e #'name))]
              [tag (format "~a:~a" str (or n 1))])
         
         (when n
           (inc-chunk-number (syntax-local-introduce #'name)))
         
         (syntax-local-lift-expression #'(quote-syntax (a-chunk name expr (... ...))))
         
         (with-syntax ([tag tag]
                       [str str]
                       [((for-label-mod (... ...)) (... ...))
                        (map (lambda (expr)
                               (syntax-case expr (require)
                                 [(require mod (... ...))
                                  (let loop ([mods (syntax->list #'(mod (... ...)))])
                                    (cond
                                      [(null? mods) null]
                                      [else 
                                       (syntax-case (car mods) (for-syntax)
                                         [(for-syntax x (... ...))
                                          (append (loop (syntax->list #'(x (... ...))))
                                                  (loop (cdr mods)))]
                                         [x
                                          (cons #'x (loop (cdr mods)))])]))]
                                 [else null]))
                             (syntax->list #'(expr (... ...))))]
                       
                       [(rest (... ...)) (if n
                                             #`((subscript #,(format "~a" n)))
                                             #`())])
           #`(begin
               (require (for-label for-label-mod (... ...) (... ...)))
               #,@(if n
                      #'()
                      #'((define-syntax name (make-element-id-transformer
                                              (lambda (stx) #'(chunkref name))))
                         (begin-for-syntax (init-chunk-number #'name))))
               ;(make-splice
               ;(list (make-toc-element
               ;#f
               ;(list (elemtag '(chunk tag)
               ;               (bold (italic (racket name)) " ::=")))
               ;(list (smaller (elemref '(chunk tag) #:underline? #f
               ;                        str
               ;                        rest (... ...)))))
               (racket expr (... ...)))))]))) ;))

(make-chunk chunk2 racketblock)
(make-chunk CHUNK2 RACKETBLOCK)

(define-syntax (chunkref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([tag (format "~a:1" (syntax-e #'id))]
                   [str (format "~a" (syntax-e #'id))])
       #'(elemref '(chunk tag) #:underline? #f str))]))

(provide chunk2 CHUNK2)

(provide tc TC)
(define-syntax-rule (tc . rest) (chunk2 name . rest))
(define-syntax-rule (TC . rest) (CHUNK2 name . rest))
