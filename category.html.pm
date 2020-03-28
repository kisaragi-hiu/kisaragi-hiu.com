#lang pollen
◊define-meta[type]{page}
◊define-meta[title]{Categories}

◊(current-pagetree "index.ptree")
◊(require "rkt/category.rkt")

◊ul{
◊(for/splice ([category (get-categories (children 'blog))])
   (let ([uri (abs-local (category-path category))]
         [title (select-from-metas 'title (string->symbol (category-path category)))])
      `(li ,(link uri title))))
}
