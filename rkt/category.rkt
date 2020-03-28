#lang racket

(require threading
         "post.rkt")

(provide (all-defined-out))

(define (get-categories pagenodes)
  (~>> (map post-category pagenodes)
       remove-duplicates
       (filter string?)))

;; "id" form of category used in file names
(define (category-id cat)
  (~> (string-downcase cat)
      (string-replace " " "-")))

;; output file name of category index
(define (category-path cat)
  (format "category/~a.html"
          (category-id cat)))

;; display form of category
(define (category-display cat)
  (~> (string-titlecase cat)
      (string-replace "-" " ")))
