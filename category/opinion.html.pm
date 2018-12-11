#lang pollen
◊(require threading)
◊(current-pagetree "../index.ptree")

◊define-meta[type]{category-index}

◊(~> (children 'blog (current-pagetree))
     (filter
      (curry in-category? "opinion")
      _)
     (map index-item _)
     (txexpr 'div '([class "index"]) _))
