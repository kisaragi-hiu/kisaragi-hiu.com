#lang racket/base

(require racket/list
         racket/string
         threading)

(provide (all-defined-out))

;; Given + (adjacent sibling operator), "h1,h2", and "h3,h4",
;; return "h1+h3,h1+h4,h2+h3,h2+h4".
;; This is used to match headings next to headings.
;; list1 and list2 can be strings separated by "," or lists.
(define (css-op-all operation . lists)
  (define (coerce-to-list x)
    (cond
      ((string? x) (map string-trim (string-split x ",")))
      (else x)))
  (~> (map coerce-to-list lists)
      (apply cartesian-product _)
      (map (lambda (x) (string-join x operation)) _)
      (string-join ",")))

;; '("a" "b" c) -> "'a', 'b', c"
;; turn a font list in lisp format into css format
;; use single quotes so we don't have to escape them
(define (to-css-list fonts)
  (string-join
   (for/list ((elem (in-list fonts)))
     (if (string? elem) (format "'~a'" elem) (format "~a" elem)))
   ", "))

(define & to-css-list)
