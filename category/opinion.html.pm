#lang pollen
◊(require threading)
◊(current-pagetree "../index.ptree")

◊define-meta[type]{category-index}

◊(~> (children 'blog (current-pagetree))
     (filter
       (lambda (p)
        (and (string? (select-from-metas 'category p))
             (string-ci=? (select-from-metas 'category p) "opinion")))
      _)
     (map index-item _)
     (txexpr 'div '([class "index"]) _))
