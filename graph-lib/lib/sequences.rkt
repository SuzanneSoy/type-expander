#lang typed/racket

(provide sequence-cons sequence-null sequence-list)

(: sequence-cons (∀ (A B) (→ (Sequenceof A) (Sequenceof B)
                             (Sequenceof (cons A B)))))
(define (sequence-cons sa sb)
  (sequence-map (λ ([x : (List A B)]) (cons (car x) (cadr x)))
                (in-values-sequence (in-parallel sa sb))))

(: sequence-null (Sequenceof Null))
(define sequence-null (in-cycle (in-value '())))

;; sequence-list should have the type:
;; (∀ (A ...) (→ (Sequenceof A) ... (Sequenceof (List A ...)))))
;; But the type system rejects the two definitions below.
(: sequence-list (∀ (A) (→ (Sequenceof A) *
                           (Sequenceof (Listof A)))))
(define (sequence-list . sequences)
  (if (null? sequences)
      sequence-null
      (sequence-cons (car sequences) (apply sequence-list (cdr sequences)))))

#|
(: sequence-list (∀ (A ...) (→ (Sequenceof A) ...
                               (Sequenceof (List A ...)))))
(define (sequence-list . sequences)
  (if (null? sequences)
      sequence-null
      (sequence-cons (car sequences) (apply sequence-list (cdr sequences)))))
|#

#|
(: sequence-list (∀ (F R ...)
                    (case→ [→ (Sequenceof Null)]
                           [→ (Sequenceof F) (Sequenceof R) ...
                              (Sequenceof (List F R ...))])))
(define sequence-list
  (case-lambda
    [()
     sequence-null]
    [(sequence . sequences)
     (sequence-cons sequence (apply sequence-list sequences))]))
|#

