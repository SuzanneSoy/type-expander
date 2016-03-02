#lang typed/racket
(require "../low2/typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (require (for-syntax syntax/parse
                       racket/syntax
                       racket/function
                       racket/match
                       syntax/stx))
  
  (provide define-logn-ids)
  
  (begin-for-syntax
    (define (insert make-node v ts)
      (match ts
        [`() `((,v))]
        [`(() . ,b) `((,v) . ,b)]
        [`((,a) . ,b) `(() . ,(insert make-node (make-node v a) b))]))
    
    (define (merge-trees make-node ts)
      (match ts
        [`{[,a]} a]
        [`{[,a] [] . ,rest} (merge-trees make-node `{[,a] . ,rest})]
        [`{[] . ,rest} (merge-trees make-node rest)]
        [`{[,a] [,b] . ,rest} (merge-trees make-node
                                           `{[,(make-node a b)] . ,rest})]))
    
    (define (make-binary-tree l make-node make-leaf)
      (merge-trees make-node
                   (foldl (curry insert make-node)
                          '()
                          (map make-leaf l)))))
  
  (define-syntax (define-logn-ids stx)
    (syntax-parse stx
      [(_ matcher:id [id:id ty:id] ...)
       (define/with-syntax (tmp ...) (generate-temporaries #'(id ...)))
       (define bt
         (make-binary-tree (syntax->list #'([ty id . tmp] ...))
                           (λ (x y) `(node ,(generate-temporary) ,x ,y))
                           (λ (x) `(leaf ,(stx-car x)
                                         ,(generate-temporary (stx-car x))
                                         ,(stx-car (stx-cdr x))
                                         ,(stx-cdr (stx-cdr x))))))
       (define (make-structs bt parent)
         (match bt
           [`(node ,s ,a ,b) #`(begin (struct #,s #,@parent ())
                                      #,(make-structs a (list s))
                                      #,(make-structs b (list s)))]
           [`(leaf ,t ,s ,a ,_) #`(begin (struct #,s #,@parent
                                           ()
                                           #:type-name #,t)
                                         (define #,a (#,s)))]))
       (define (make-btd bt)
         (match bt
           [`(node ,s ,(and a `(,_ ,sa . ,_)) ,b)
            #`(if ((make-predicate #,sa) v-cache)
                  #,(make-btd a)
                  #,(make-btd b))]
           [`(leaf ,s ,a ,t ,tmp)
            tmp]))
       #`(begin #,(make-structs bt #'())
                (define-syntax (matcher stx)
                  (syntax-parse stx
                    [(_ v:expr [(~literal id) tmp] ...)
                     #'(let ([v-cache v])
                         #,(make-btd bt))])))]))
  
  (module* test typed/racket
    (require (submod "..")
             typed/rackunit)
    
    (define-logn-ids match-x [a A] [b B] [c C] [d D] [e E])
    
    (check-equal? (match-x (ann b (U A B C D E))
                           [a 1]
                           [b 2]
                           [c 3]
                           [d 4]
                           [e 5])
                  2)))