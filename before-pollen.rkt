#lang rackjure

(require pollen/pagetree
         threading
         "rkt/post.rkt"
         "rkt/category.rkt"
         "generate-page-functions.rkt")

(current-pagetree "index.ptree")

(make-directory* "category")
(for ((cat (get-categories (children 'blog))))
  (define path (category-path cat))
  (define category (category-display cat))
  (define category-pollen-source (~a path ".pm"))
  (with-output-to-file category-pollen-source
    #:exists 'truncate
    #λ(displayln (generate-category-page category))))
