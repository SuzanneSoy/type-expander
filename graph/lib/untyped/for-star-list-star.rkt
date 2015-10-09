#lang racket

(provide for*/list*)

(require (for-syntax syntax/parse))

(define-syntax (for*/list* stx)
  (define-syntax-class sequences
    #:description "([id seq-expr] ...) or (* [id seq-expr] ...)"
    (pattern ((~optional (~and star (~datum *))) (id:id seq-expr:expr) ...)
             #:with for-kind (if (attribute star) #'for*/list #'for/list)))
  
  (syntax-parse stx
    [(_ sequences:sequences ... body)
     (foldl (λ (for-kind clauses acc)
              #`(#,for-kind #,clauses #,acc))
            #'body
            (reverse (syntax-e #'(sequences.for-kind ...)))
            (reverse (syntax-e #'(([sequences.id sequences.seq-expr] ...) ...))))]))

;; Test for*/list*
(module* test racket
  (require rackunit)
  (require (submod ".."))
  (check-equal? (for*/list* ([x '(a b c)]
                             [y '(1 2 3)])
                            (* [z '(d e f)]
                               [t '(4 5 6)])
                            (list x y z t))
                '(((a 1 d 4) (a 1 d 5) (a 1 d 6) (a 1 e 4) (a 1 e 5) (a 1 e 6) (a 1 f 4) (a 1 f 5) (a 1 f 6))
                  ((b 2 d 4) (b 2 d 5) (b 2 d 6) (b 2 e 4) (b 2 e 5) (b 2 e 6) (b 2 f 4) (b 2 f 5) (b 2 f 6))
                  ((c 3 d 4) (c 3 d 5) (c 3 d 6) (c 3 e 4) (c 3 e 5) (c 3 e 6) (c 3 f 4) (c 3 f 5) (c 3 f 6))))
  (check-equal? (for*/list* ([x '(a b c)])
                            ([y '(1 2 3)])
                            (* [z '(d e f)]
                               [t '(4 5 6)])
                            (list x y z t))
                '((((a 1 d 4) (a 1 d 5) (a 1 d 6) (a 1 e 4) (a 1 e 5) (a 1 e 6) (a 1 f 4) (a 1 f 5) (a 1 f 6))
                   ((a 2 d 4) (a 2 d 5) (a 2 d 6) (a 2 e 4) (a 2 e 5) (a 2 e 6) (a 2 f 4) (a 2 f 5) (a 2 f 6))
                   ((a 3 d 4) (a 3 d 5) (a 3 d 6) (a 3 e 4) (a 3 e 5) (a 3 e 6) (a 3 f 4) (a 3 f 5) (a 3 f 6)))
                  (((b 1 d 4) (b 1 d 5) (b 1 d 6) (b 1 e 4) (b 1 e 5) (b 1 e 6) (b 1 f 4) (b 1 f 5) (b 1 f 6))
                   ((b 2 d 4) (b 2 d 5) (b 2 d 6) (b 2 e 4) (b 2 e 5) (b 2 e 6) (b 2 f 4) (b 2 f 5) (b 2 f 6))
                   ((b 3 d 4) (b 3 d 5) (b 3 d 6) (b 3 e 4) (b 3 e 5) (b 3 e 6) (b 3 f 4) (b 3 f 5) (b 3 f 6)))
                  (((c 1 d 4) (c 1 d 5) (c 1 d 6) (c 1 e 4) (c 1 e 5) (c 1 e 6) (c 1 f 4) (c 1 f 5) (c 1 f 6))
                   ((c 2 d 4) (c 2 d 5) (c 2 d 6) (c 2 e 4) (c 2 e 5) (c 2 e 6) (c 2 f 4) (c 2 f 5) (c 2 f 6))
                   ((c 3 d 4) (c 3 d 5) (c 3 d 6) (c 3 e 4) (c 3 e 5) (c 3 e 6) (c 3 f 4) (c 3 f 5) (c 3 f 6))))))