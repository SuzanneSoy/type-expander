#lang typed/racket

(provide make/proc
         argv
         string-prefix?
         string-suffix?
         find-files-by-extension
         t-rule
         rules
         ;rule
         implicit-rule
         for/rules
         path-string->string
         path-string->path
         path-append
         regexp-case
         dirname
         underscore-extension
         compile-zos
         rkt->zo-dir
         rkt->zo-file
         make-collection
         run
         run!
         find-executable-path-or-fail)

(require/typed make [make/proc (→ (Listof
                                   (Pairof (U Path-String (Listof Path-String))
                                           (Pairof (Listof Path-String)
                                                   (U Null
                                                      (List (-> Any))))))
                                  (U String (Vectorof String) (Listof String))
                                  Void)])
;(require/typed make/collection [make-collection (→ Any (Listof Path-String)
;                                                   (U String (Vectorof String))
;                                                   Void)])

(require/typed srfi/13
               [string-suffix? (->* (String String)
                                    (Integer Integer Integer Integer)
                                    Boolean)]
               [string-prefix? (->* (String String)
                                    (Integer Integer Integer Integer)
                                    Boolean)])

(define (find-files-by-extension [ext : String])
  (find-files (λ ([path : Path]) (string-suffix? ext (path->string path)))))

;(: drop-extension (→ Path-String * String String))
;(define (drop-extension path . exts)
;  (map (λ () exts)

(define-type t-rule (Pairof (U Path-String (Listof Path-String))
                            (Pairof (Listof Path-String)
                                    (U Null
                                       (List (-> Any))))))
(: rules (→ (U t-rule (Listof t-rule)) * (Listof t-rule)))
(define (rules . rs)
  (apply append (map (λ ([x : (U t-rule (Listof t-rule))])
                       (cond
                         ;; x = '() is an empty (Listof t-rule)
                         [(null? x) '()]
                         ;; x = '([target dep maybe-proc]) is a (Listof t-rule)
                         ;;     with just one element
                         [(null? (cdr x)) x]
                         ;; x = '[target () maybe-proc] is a t-rule
                         ;;     with an empty list of dependencies
                         [(null? (cadr x)) (list x)]
                         ;; Below, either x = '[target (dep₁ . ?) maybe-proc]
                         ;;        or x = '([target dep maybe-proc]
                         ;;                 [target dep maybe-proc])
                         [else (cond
                                 ;; x = '[target (dep₁ . ()) maybe-proc]
                                 [(null? (cdadr x)) (list x)]
                                 ;; x = '([target dep maybe-proc]
                                 ;;       [target (?) maybe-proc])
                                 [(list? (cadadr x)) x]
                                 ; x = '[target (dep₁ dep₂ . ()) maybe-proc]
                                 [else (list x)])]))
                     rs)))

#|
(define-syntax (rule stx)
  (syntax-case stx ()
    [(_ (target ...) (depend ...) body0 . body)
     #'(list (list target ...)
             (list depend ...)
             (λ () body0 . body))]
    [(_ (target ...) (depend ...))
     #'(list (list target ...)
             (list depend ...))]))
|#

(define-syntax-rule (implicit-rule (arg ...) (target ...) (depend ...) body ...)
  (λ ([arg : Path] ...)
    (list (list target ...)
          (list depend ...)
          (λ () body ...))))

(define-syntax-rule (for/rules ([arg files] ...) (target ...) (depend ...)
                      body ...)
  (map (implicit-rule (arg ...) (target ...) (depend ...) body ...) files ...))

(: path-string->string (→ Path-String String))
(define (path-string->string ps)
  (if (string? ps)
      ps
      (path->string ps)))

(: path-string->path (→ Path-String Path))
(define (path-string->path ps)
  (if (string? ps)
      (string->path ps)
      ps))

(: path-append (→ Path-String Path-String Path))
(define (path-append a b)
  (string->path (string-append (path-string->string a)
                               (path-string->string b))))

(define-syntax-rule (regexp-case input [pattern replacement] ...)
  (let ([input-cache input]) ;; TODO: should also cache the patterns, but lazily
    (cond
      [(regexp-match pattern input-cache)
       (regexp-replace pattern input-cache replacement)]
      ...
      [else
       input-cache])))

(: dirname (→ Path Path))
(define (dirname p)
  (let-values ([(base name must-be-dir?) (split-path p)])
    (case base
      ['relative (build-path 'same)] ;; If the path is "..", then this is wrong.
      ['#f (error (format "Can't get parent directory of ~a" p))]
      [else base])))

(: underscore-extension (→ Path-String String * Path))
(define (underscore-extension path . ext)
  (if (null? ext)
      (path-string->path path)
      (let ([p (path-string->string path)]
            [e (path-string->string (car ext))])
        (if (string-suffix? (car ext) p)
            (let* ([pos (- (string-length p) (string-length e))]
                   [left (substring p 0 pos)])
              (string->path (string-append left (string-replace e "." "_"))))
            (apply underscore-extension path (cdr ext))))))

;; TODO: do pattern-matching on paths, with (match) ?

(define (argv)
  (let ([argv (current-command-line-arguments)])
    (if (= (vector-length argv) 0)
        #("zo")
        argv)))

;; make-collection copied from the file
;; /usr/local/racket-6.2.900.6/share/pkgs/make/collection-unit.rkt
(require/typed compiler/compiler
               [compile-zos (->* (Any)
                                 (#:module? Any #:verbose? Any)
                                 (→ (Listof Path-String)
                                    (U Path-String #f 'auto)
                                    Void))])
(require/typed dynext/file
               [append-zo-suffix (→ Path-String Path)])

(: cache (∀ (T) (→ (→ T) (→ T))))
(define (cache producer)
  ;; Use (List T) instead of T, so that if the producer returns #f,
  ;; we don't call it each time.
  (let ([cache : (U False (List T)) #f])
    (λ ()
      ;; since cache is mutated by set!, occurrence typing won't work on it,
      ;; so we need to take a copy:
      (let ([c cache])
        (if c
            (car c)
            (let ((producer-result (producer)))
              (set! cache (list producer-result))
              producer-result))))))

(: rkt->zo-dir (→ Path-String Path))
(define (rkt->zo-dir src-file)
  (simplify-path (build-path src-file 'up "compiled") #f))

(: rkt->zo-file (→ Path-String Path))
(define (rkt->zo-file src-file)
  (build-path (rkt->zo-dir src-file)
              (append-zo-suffix (assert (file-name-from-path src-file)))))

(: make-collection  (→ Any (Listof Path-String) (U String (Vectorof String))
                       Void))
(define (make-collection collection-name collection-files argv)
  (printf "building collection ~a: ~a\n" collection-name collection-files)
  (let* ([zo-compiler (cache (λ () (compile-zos #f)))]
         [src-dir (current-directory)]
         [rkts (sort collection-files
                     (lambda ([a : Path] [b : Path])
                       (string-ci<? (path->string a) (path->string b))))]
         [zos (map (lambda ([rkt : Path-String])
                     (rkt->zo-file rkt))
                   rkts)]
         [rkt->zo-list
          (map (lambda ([rkt : Path-String] [zo : Path])
                 `(,zo (,rkt)
                       ,(lambda ()
                          (let ([dest (rkt->zo-dir rkt)])
                            (unless (directory-exists? dest)
                              (make-directory dest))
                            ((zo-compiler) (list rkt) dest)))))
               rkts zos)])
    (make/proc (append `(("zo" ,zos)) rkt->zo-list) argv)))

(: run (→ (U Path-String (Pairof Path-String (Listof (U Path-String Bytes))))
          [#:set-pwd? Any]
          (U Path-String Bytes) *
          Boolean))
(define (run arg0 #:set-pwd? [set-pwd? #f] . args)
  (if (list? arg0)
      (apply run arg0)
      (begin
        (displayln (string-join (cons (path-string->string arg0)
                                      (map (λ (x) (format "~a" x)) args))
                                " "))
        (display "\033[1;34m")
        (flush-output)
        (let ((exit-code (apply system*/exit-code arg0 args)))
          (display "\033[m")
          (flush-output)
          (unless (= exit-code 0)
            (raise (format "Command failed with exit code ~a." exit-code)))
          (= exit-code 0)))))

(define-syntax-rule (run! . rest) (let () (run . rest) (values)))

(: find-executable-path-or-fail (->* (Path-String)
                                     ((U Path-String False) Any)
                                     Path))
(define find-executable-path-or-fail
  (let ((fn (λ ([executable-name : Path-String]
                [a : (U Path-String False 'none)]
                [b : (U (List Any) 'none)])
              : Path
              (or (if (eq? a 'none)
                      (find-executable-path executable-name)
                      (if (eq? b 'none)
                          (find-executable-path executable-name a)
                          (find-executable-path executable-name a (car b))))
                  (error (format "Can't find executable '~a'"
                                 executable-name))))))
    (case-lambda
      [(x) (fn x 'none 'none)]
      [(x a) (fn x a 'none)]
      [(x a b) (fn x a (list b))])))
