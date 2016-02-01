#lang s-exp "list-lang.rkt"

(provide all-remembered-list all-remembered-alist)

;; For older versions:
#|
(require/typed unstable/list
               [(group-by group-by-untyped)
                (∀ (A B) (->* [(→ A B) (Listof A)]
                              [(→ B B Any)]
                              (Listof (Listof A))))])

;; Circumvent problem with contracts: with the imported untyped version,
;; two identical keys are never equal?. I think this is because  the keys are
;; wrapped with contracts when they are passed to equal?, and therefore are not
;; equal?, because we're comparing two distinct contract wrappers, instead of
;; comparing their contents.
(: group-by (∀ (A B) (->* [(→ A B) (Listof A)]
                          [(→ B B Any)]
                          (Listof (Listof A)))))
(define (group-by key lst [same? equal?])
  (group-by-untyped key lst (λ ([x : B] [y : B]) (same? x y))))
|#

(define all-remembered-alist
  (map (λ ([g : (Listof (Pairof Symbol Any))]) : (Pairof Symbol (Listof Any))
         (cons (caar g) (remove-duplicates (map (inst cdr Symbol Any) g))))
       (group-by (inst car Symbol Any) all-remembered-list)))

(define-list-values all-remembered-list : (Listof (Pairof Symbol Any)))
(structure a b c)
(structure a b c d)
(structure a b c y)
(structure a b)
(structure x y z)
(structure)
(structure f g)
(structure faa fab fav)
(structure fba fbv)
(structure fav)
(structure a)
(structure a)
(structure people/with-promises-type streets/with-promises-type)
(structure houses/with-promises-type name/with-promises-type)
(structure location/with-promises-type owner/with-promises-type)
(structure name/with-promises-type)
(structure people streets)
(structure people streets)
(structure houses name)
(structure houses name)
(structure location owner)
(structure location owner)
(structure name)
(structure name)
(structure houses/with-promises-type sname/with-promises-type)
(structure houses sname)
(structure houses sname)
(structure f-a f-b)
(structure f-a f-b f-c)
(structure people)
(structure streets)
(structure location)
(structure streets)
(structure streets)
(structure x)
(structure x)
(structure x)
(structure sname)
(structure sname)
(structure sname)
(structure st)
(structure v)
(structure v)
(structure b1 b2 v)
(structure a s v)
(structure b1 b2 v)
(structure b1 b2 v)
(structure v w)
(structure v w)
(structure v w)
