#lang typed/racket

(provide (struct-out protected)
         unprotect
         cond-abort
         match-abort
         let-abort
         let*-abort
         map-abort)

(struct (A) protected ([value : A]))

(define-syntax (unprotect stx)
  (syntax-case stx ()
    [(_ e0 . e)
     (with-syntax ([(tmp) (generate-temporaries #'(e0))])
       #'(let ([tmp (let () e0 . e)])
           (if (protected? tmp)
               (protected-value tmp)
               tmp)))]))

(define-syntax (cond-abort orig-stx)
  (let rec ([stx orig-stx])
    (syntax-case stx (else =>)
      [(_)
       #`(typecheck-fail #,orig-stx)]
      [(_ [else body0 . body])
       #`(let () body0 . body)]
      [(_ [condition] . rest)
       ;; TODO: this does not work when body returns multiple values.
       #`(let ([result condition])
           (if (or (not result) (eq? result 'continue))
               #,(rec #'(cond-abort . rest))
               (if (eq? result 'break)
                   'continue
                   result)))]
      [(_ [condition body0 . body] . rest)
       ;; TODO: this does not work when body returns multiple values.
       #`(let ([result (if condition
                           (let () body0 . body)
                           'continue)])
           (if (eq? result 'continue)
               #,(rec #'(cond-abort . rest))
               (if (eq? result 'break)
                   'continue
                   result)))])))

(define-syntax-rule (match-abort v [pattern body0 . body] ...)
  (let ([v-cache v])
    (cond-abort
     [#t (match v-cache
           [pattern (let () body0 . body)]
           [_ 'continue])]
     ...)))

(define-syntax-rule (let-abort ([binding value] ...) . body)
  (let ([binding value] ...)
    (cond
      [(or (eq? 'continue binding) (eq? 'break binding)) binding]
      ...
      [else (let () . body)])))

(define-syntax (let*-abort stx)
  (syntax-case stx ()
    [(_ () . body)
     #'(begin . body)]
    [(_ ([binding0 value0] . rest) . body)
     #'(let ([binding0 value0])
         (if (or (eq? 'continue binding) (eq? 'break binding))
             binding
             (let*-abort rest . body)))]))

(define-syntax (map-abort stx)
  (syntax-case stx ()
    [(_ lst var . body)
     #'(let ([l lst])
         (if (null? l)
             ;; Special-case to avoid type inference issues with an empty
             ;; result-list.
             '()
             (let ([result-list (list (let ([var (car l)]) . body))])
               (set! l (cdr l))
               (do ([stop : (U #f #t 'continue 'break)
                          #f])
                 (stop (if (or (eq? stop 'continue) (eq? stop 'break))
                           stop
                           (reverse result-list)))
                 (if (null? l)
                     (set! stop #t)
                     (let ([result (let ([var (car l)]) . body)])
                       (if (or (eq? result 'continue) (eq? result 'break))
                           (set! stop result)
                           (begin
                             (set! result-list (cons result result-list))
                             (set! l (cdr l))))))))))]))
