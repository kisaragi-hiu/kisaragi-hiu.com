#lang pollen
◊(require threading)
◊(current-pagetree "../index.ptree")

◊define-meta[title]{opinion}
◊define-meta[type]{category-index}

◊(~> (children 'blog (current-pagetree))
     (filter
      (curryr in-category? "opinion")
      _)
     (map index-item _)
     (txexpr 'div '([class "index"]) _))
