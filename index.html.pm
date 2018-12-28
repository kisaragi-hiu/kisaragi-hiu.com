#lang pollen
◊define-meta[title]{Home}
◊define-meta[type]{root-index}

◊(current-pagetree "index.ptree")

◊(define blog-entries (children 'blog "index.ptree"))
◊(define blog-entry-years
   (remove-duplicates (map post-year blog-entries)))

◊(for/splice ([year blog-entry-years])
   `(div ([class "index-year"])
      ,(heading (number->string year))
      ,(pagetree-to-index (filter (curryr post-year=? year) blog-entries))))
