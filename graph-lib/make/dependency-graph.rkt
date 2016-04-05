#lang racket

(require macro-debugger/analysis/show-dependencies)

(provide print-dep-graph
         get-dep-graph)

(define (categorize x)
  (match x
    [(? symbol?
        (? (λ (x) (regexp-match #px"^phc-toolkit(/|$)" (symbol->string x)))))
     'phc-toolkit]
    #;[(? (λ (x) (regexp-match #px"phc-toolkit" (format "~a" x))))
     'phc-toolkit]
    [(? symbol?) 'lib]
    [(list 'quote (? symbol?
                     (? (λ (x) (regexp-match #rx"^#%" (symbol->string x))))))
     'lib]
    [(list 'submod sub _ ...) (categorize sub)]
    [(list (? symbol? s) _ ...) s]
    [_ #f]))

(define (lib? x)
  (equal? (categorize x) 'lib))

(define (categorize-main-module x)
  (match x
    [(? symbol? s) s]
    [(list 'file (? string? s)) s]
    [(list 'submod s _ ...) (categorize-main-module s)]
    [(list (? symbol? s) _ ...) s]
    [_ x]))

(define (simplify x)
  (match x
    [(list 'file (? string? s)) s]
    [_ x]))

(define (deps->graphviz dep-pairs [show-inner #f])
  (define (show m)
    (parameterize ([pretty-print-columns 'infinity])
      (write (pretty-format ((or show-inner identity) m)))))
  
  (define all-modules
    (remove-duplicates (append (map car dep-pairs)
                               (map cdr dep-pairs))))
  
  (printf "digraph deps {\n")
  (for ([mg (in-list (group-by categorize-main-module all-modules))]
        [i (in-naturals)])
    (when (> (length mg) 1)
      (printf "  subgraph cluster_~a {\n" i)
      (printf "    style=filled;\n")
      (printf "    color=lightgrey;\n")
      (for ([m (in-list mg)])
        (printf "    ")
        (show m)
        (printf ";\n"))
      (printf "  }\n")))
  (for ([dep (in-list dep-pairs)])
    (printf "  ")
    (show (car dep))
    (printf " -> ")
    (show (cdr dep))
    (printf ";\n"))
  (printf "}\n"))

(define excluded
  '(typed/racket
    typed/racket/no-check
    racket/base
    racket
    scribble/lp/lang/lang2))

(define (print-dep-graph source-file)
  (define rdeps
    (get-dependencies `(file ,source-file)
                      #:exclude-deps excluded))
  
  (define dep-pairs
    (append-map (λ (rdep)
                  (let ([mod (car rdep)]
                        [direct-requirers (cadr rdep)])
                    (for/list ([direct-requirer (in-list direct-requirers)])
                      (cons direct-requirer mod))))
                rdeps))
  
  (define deps
    (map (λ (g)
           (cons (caar g) (append-map cdr g)))
         (group-by car
                   (append (map (λ (dep) (list (car dep) (cdr dep))) dep-pairs)
                           (map (compose list cdr) dep-pairs)))))
  
  (define grouped-deps
    (map (λ (g)
           (list (categorize-main-module (caar g))
                 (map car g)
                 (remove-duplicates (append-map cdr g))))
         (group-by (compose categorize-main-module car) deps)))
  
  (define (get-tags grouped-dep)
    (append
     (if (> (length (cadr grouped-dep)) 1) '(submodule) '())
     (if (member 'typed/racket (caddr grouped-dep))
         '(typed/racket)
         (if (ormap lib? (cadr grouped-dep))
             '()
             '(untyped)))
     (if (member 'scribble/lp/lang/lang2 (caddr grouped-dep)) '(scribble) '())))
  
  (define tagged-mods
    (map (λ (grouped-dep) ; (req dep …) …
           (let* ([tags (get-tags grouped-dep)])
             (cons (car grouped-dep)
                   tags)))
         grouped-deps))
  
  (define (typed-racket-internals m)
    (define (rx sym) (regexp-match #rx"^typed-racket/" (symbol->string sym)))
    (match m
      [(? symbol? s) (rx s)]
      [(list 'submod (? symbol? s) _ ...) (rx s)]
      [_ #f]))
  
  (define (tag-pair dep)
    (append (if (eq? (categorize (cdr dep)) 'phc-toolkit)
                '(phc-toolkit)
                '())
            (if (equal? (categorize-main-module (car dep))
                        (categorize-main-module (cdr dep)))
                '(submodule)
                '())
            (if (lib? (cdr dep))
                '(lib)
                '())))
  
  (define tagged-dep-pairs
    (map (λ (dep)
           (cons dep (tag-pair dep)))
         dep-pairs))
  
  (define filtered-tagged-dep-pairs
    (filter-map (λ (dep)
                  (and (not (lib? (caar dep)))
                       (not (typed-racket-internals (cdar dep)))
                       (not (member (cdar dep) excluded))
                       dep))
                tagged-dep-pairs))
  
  (define categorized-tagged-dep-pairs
    (map (λ (deps) (cons (caar deps) (remove-duplicates (append-map cdr deps))))
         (group-by car
                   (map (λ (dep)
                          (cons (cons (categorize-main-module (caar dep))
                                      (categorize-main-module (cdar dep)))
                                (cdr dep)))
                        filtered-tagged-dep-pairs))))
  
  (define filtered-tagged-mods
    (filter-map (λ (m) (assoc m tagged-mods))
                (remove-duplicates
                 (append (map caar categorized-tagged-dep-pairs)
                         (map cdar categorized-tagged-dep-pairs)))))
  
  #;(define (mod-tags->styles ts)
      (define kv
        (for/fold ([h (hash)])
                  ([t (in-list ts)])
          (case t
            [(submodule) '(hash-set* h
                                     "style" "filled"
                                     "line-style" "double")]
            [(typed/racket) (hash-set* h
                                       "color" "green")]
            [(scribble) (hash-set* h
                                   "shape" "box"
                                   "style" "rounded")]
            [else h])))
      (filter identity
              (for/list ([(key value) (in-hash kv)])
                (case key
                  [("line-style") (let ([c (hash-ref kv "color"
                                                     (λ () "black"))])
                                    (if (equal? value "double")
                                        (format "color=\"~a:invis:~a\"" c c)
                                        c))]
                  [("color") (if (hash-has-key? kv "line-style")
                                 #f
                                 (format "~a=~a" key value))]
                  [else (format "~a=~a" key value)]))))
  
  (define (mod-tag->style t)
    (case t
      [(submodule) "peripheries=2"]
      [(typed/racket) "color=\"#44cc44\""]
      [(scribble) "shape=box, style=rounded"]
      [(untyped) "color=\"#bb2222\""]
      [(()) ""]
      [else #f]))
  
  (define (dep-tag->style t)
    (case t
      [(phc-toolkit) "color=grey,style=dotted"]
      [(submodule) "color=grey"]
      [(lib) "style=dashed"]
      [else #f]))
  
  (define (deps->graphviz2 tagged-mods tagged-dep-pairs)
    (define (show m)
      (parameterize ([pretty-print-columns 'infinity])
        (write (pretty-format m))))
    
    (printf "digraph deps {\n")
    (printf "  nodesep=0.1\n")
    (printf "  node[shape=box]\n")
    (for ([mod (in-list tagged-mods)])
      (printf "  ")
      (show (car mod))
      (printf " [~a]"
              (if (null? (cdr mod))
                  (mod-tag->style '())
                  (string-join (filter-map mod-tag->style (cdr mod)) ", ")))
      (printf ";\n"))
    (for ([dep (in-list tagged-dep-pairs)])
      (printf "  ")
      (show (caar dep))
      (printf " -> ")
      (show (cdar dep))
      (printf " [~a]" (string-join (filter-map dep-tag->style (cdr dep)) ", "))
      (printf ";\n"))
    (printf "}\n"))
  
  (deps->graphviz2 filtered-tagged-mods categorized-tagged-dep-pairs))

(define (get-dep-graph source-file)
  (let ((o (open-output-string "graph.dot")))
    (parameterize ([current-output-port o])
      (print-dep-graph source-file)
      (get-output-string o))))

(module* main racket
  (require (submod ".."))
  
  (let ([argv (current-command-line-arguments)])
    (if (not (= (vector-length argv) 2))
        #;(error (format "Got ~a arguments but expected 2: ~a"
                         (vector-length argv)
                         "source and output files"))
        (void)
        (let ()
          (define source-file (vector-ref argv 0))
          (define output-file (vector-ref argv 1))
          (parameterize ([current-output-port
                          (open-output-file output-file #:exists 'replace)])
            (print-dep-graph source-file))))))
