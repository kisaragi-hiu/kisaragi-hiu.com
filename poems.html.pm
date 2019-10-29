#lang pollen
◊define-meta[title]{Poems}
◊define-meta[type]{index}

◊(current-pagetree "index.ptree")

◊(define entries (children 'poems "index.ptree"))
◊(define entry-years
   (remove-duplicates (map post-year entries)))

◊(pagetree-to-index (txexpr 'root empty entries))

◊;(for/splice ([year entry-years])
◊;  `(div ([class "index-year"])
◊;     ,(heading (number->string year))
◊;     ,(pagetree-to-index
◊;       (txexpr 'root empty (filter (curryr post-year=? year) entries)))))
