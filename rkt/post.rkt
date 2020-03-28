#lang racket

(require pollen/core
         pollen/pagetree)

(provide in-category?
         post-year
         post-year=?
         post-type)

;; get type of current post
(define (post-type metas)
  (or (select-from-metas 'type metas)
      "post"))

(define/contract (post-year pagenode)
  (-> (or/c pagenode? pagenodeish?)
      number?)
  (string->number
   (substring (select-from-metas 'date pagenode)
              0 4)))

(define/contract (post-year=? pagenode year)
  (-> (or/c pagenode? pagenodeish?) number?
      boolean?)
  (= year (post-year pagenode)))

(define (in-category? pagenode category)
  (define cat (select-from-metas 'category pagenode))
  (and (string? cat)
       (string-ci=? cat category)))
