#lang racket

(require threading
         "post.rkt")

(provide (all-defined-out))

(define (get-tags pagenodes)
  (~>> (map post-tags pagenodes)
       flatten
       remove-duplicates
       (filter string?)))

;; "id" form of category used in file names
(define (tag-id cat)
  (~> (string-downcase cat)
      (string-replace " " "-")))

;; output file name of category index
(define (tag-path cat)
  (format "tags/~a.html"
          (tag-id cat)))
