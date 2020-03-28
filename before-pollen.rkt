#lang rackjure

(require pollen/pagetree
         threading
         "generate-page-functions.rkt")

(current-pagetree "index.ptree")

(make-directory* "category")
(for ((cat (children 'category)))
  (define category-path (symbol->string cat))
  (define category (~> category-path
                       (string-replace _ #rx"^.*/" "")
                       (string-replace _ #rx"\\.html$" "")
                       (string-replace _ "-" " ")))
  (define category-pollen-source (~a category-path ".pm"))
  (with-output-to-file category-pollen-source
    #:exists 'truncate
    #λ(displayln (generate-category-page category))))
