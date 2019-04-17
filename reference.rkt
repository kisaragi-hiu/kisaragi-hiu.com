#lang racket
;; Reference System
;; ◊ref{Something}: add Something to refs
;; ◊ref{Another Thing}: add Another Thing to refs
;; ◊reftxt{expl1}: explanation for Something
;; ◊reftxt{expl2}: explanation for Another Thing
;;
;; Wrap ◊reftxt forms in ◊references{} for better styling.

(require racket/format)

(provide ref
         reftxt
         references)

(define current-refs (make-parameter '()))
(define current-ref-# (make-parameter 1))
(struct reference (id text))

(define (ref (text ""))
  (define id (current-ref-#))
  ;; insert at last
  (current-refs (reverse (current-refs)))
  (current-refs (cons (reference id text) (current-refs)))
  (current-refs (reverse (current-refs)))
  (current-ref-# (add1 (current-ref-#)))
  `(a ([id ,(format "ref-~a" id)]
       [href ,(format "#ref-desc-~a" id)])
    ,text (sup "[" ,(~a (length (current-refs))) "]")))

(define (reftxt . desc)
  (define this-ref (car (current-refs)))
  (define this-id (reference-id this-ref))
  (define text "")
  (current-refs (cdr (current-refs)))
  (unless (string=? "" (reference-text this-ref))
    (set! text `(strong ,(~a (reference-text this-ref) ": "))))
  `(p ([class "ref-desc"]
       [id ,(format "ref-desc-~a" this-id)])
    (a ([href ,(format "#ref-~a" this-id)])
     "[" ,(~a (reference-id this-ref)) "] ")
    ,text
    ,@desc))

(define (references . expr)
  `(div ([id "references"])
    ,@expr))
