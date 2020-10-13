#lang racket/base

(require pollen/pagetree
         racket/file
         racket/format
         "rkt/generate-page-functions.rkt"
         "rkt/tags.rkt")

(current-pagetree "index.ptree")

(make-directory* "tag")
(for ((tag (get-tags (children 'blog))))
  (define path (tag-path tag))
  (define tag-pollen-source (~a path ".pm"))
  (with-output-to-file tag-pollen-source
    #:exists 'truncate
    (lambda () (displayln (generate-category-page tag)))))
