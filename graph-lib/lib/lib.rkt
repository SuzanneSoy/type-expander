#lang typed/racket

(require "low.rkt")
(require "eval-get-values.rkt")

(provide (all-from-out "low.rkt")
         (all-from-out "eval-get-values.rkt"))

;; Types
(provide AnyImmutable)
;; Functions
(provide (rename-out [∘ compose]))
;; Macros
;(provide mapp)
(provide comment)

(require (for-syntax syntax/parse
                     racket/syntax))

(define-syntax (comment stx)
  #'(values))

(define-type AnyImmutable (U Number
                             Boolean
                             True
                             False
                             String
                             Keyword
                             Symbol
                             Char
                             Void
                             ;Input-Port   ;; Not quite mutable, nor immutable.
                             ;Output-Port  ;; Not quite mutable, nor immutable.
                             ;Port         ;; Not quite mutable, nor immutable.
                             
                             ;; I haven't checked the mutability of the ones
                             ;; inside in the #||# comments below
                             #|
                             Path
                             Path-For-Some-System
                             Regexp 
                             PRegexp
                             Byte-Regexp
                             Byte-PRegexp
                             Bytes
                             Namespace
                             Namespace-Anchor
                             Variable-Reference
                             |#
                             Null
                             #|
                             EOF
                             Continuation-Mark-Set
                             |#
                             ;; We definitely don't Undefined, it's not mutable
                             ;; but it's an error if present anywhere 99.9% of
                             ;; the time. Typed/racket is moving towards making
                             ;; occurrences of this type an error, anyway.
                             ; Undefined
                             #|
                             Module-Path
                             Module-Path-Index
                             Resolved-Module-Path
                             Compiled-Module-Expression
                             Compiled-Expression
                             Internal-Definition-Context
                             Pretty-Print-Style-Table
                             Special-Comment
                             Struct-Type-Property
                             Impersonator-Property
                             Read-Table
                             Bytes-Converter
                             Parameterization
                             Custodian
                             Inspector
                             Security-Guard
                             UDP-Socket ;; Probably not
                             TCP-Listener ;; Probably not
                             Logger ;; Probably not
                             Log-Receiver ;; Probably not
                             Log-Level
                             Thread
                             Thread-Group
                             Subprocess
                             Place
                             Place-Channel
                             Semaphore ;; Probably not
                             FSemaphore ;; Probably not
                             Will-Executor
                             Pseudo-Random-Generator
                             Path-String
                             |#
                             (Pairof AnyImmutable AnyImmutable)
                             (Listof AnyImmutable)
                             ; Plus many others, not added yet.
                             ;; Don't include closures, because they can contain
                             ;; mutable variables, and we can't eq? them.
                             ; -> 
                             ; maybe Prefab? Or are they mutable?
                             ))

#|
(define-syntax (mapp stx)
  (syntax-parse stx
    [(_ var:id lst:expr body ...)
     #'(let ((l lst))
         (if (null? l)
             '()
             (let ((result (list (let ((var (car l)))
                                   body ...))))
               (set! l (cdr l))
               (do ([stop : Boolean #f])
                 (stop (reverse result))
                 (if (null? l)
                     (set! stop #t)
                     (begin
                       (set! result
                             (cons (let ((var (car l)))
                                     body ...)
                                   result))
                       (set! l (cdr l))))))))]))
|#


;; TODO: this does not work, because Null is (Listof Any)
; (mapp x (cdr '(1)) (* x x))

;; TODO: foldll
(define-syntax (foldll stx)
  (syntax-parse stx
    [(_ var:id acc:id lst:expr init:expr body ...)
     #'(let ((l lst))
         (if (null? l)
             '()
             (let ((result (list (let ((var (car l)))
                                   body ...))))
               (set! l (cdr l))
               (do ([stop : Boolean #f])
                 (stop (reverse result))
                 (if (null? l)
                     (set! stop #t)
                     (begin
                       (set! result
                             (cons (let ((var (car l)))
                                     body ...)
                                   result))
                       (set! l (cdr l))))))))]))

