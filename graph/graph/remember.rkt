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
