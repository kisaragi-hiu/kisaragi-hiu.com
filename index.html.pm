#lang pollen
◊define-meta[title]{Home}
◊define-meta[type]{root-index}

◊(current-pagetree "index.ptree")

◊(define blog-entries (children 'blog "index.ptree"))
◊(define blog-entry-years
   (remove-duplicates (map post-year blog-entries)))

I'm a college student interested in Free Software, programming, VOCALOID / UTAU culture, and language learning.

◊for/splice[((year blog-entry-years))]{
  ◊div[#:class "index-year"]{
    ◊heading[(number->string year)]
    ◊(txexpr 'div '([class "index"])
             (map (lambda (entry) (index-item entry #:year? #f))
                  (filter (curryr post-year=? year) blog-entries)))}}
