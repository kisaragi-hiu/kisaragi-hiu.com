#lang racket/base

(require pollen/pagetree
         racket/file
         racket/format
         "rkt/generate-page-functions.rkt"
         "rkt/category.rkt")

(current-pagetree "index.ptree")

(make-directory* "category")
(for ((cat (get-categories (children 'blog))))
  (define path (category-path cat))
  (define category (category-display cat))
  (define category-pollen-source (~a path ".pm"))
  (with-output-to-file category-pollen-source
    #:exists 'truncate
    (lambda () (displayln (generate-category-page category)))))
