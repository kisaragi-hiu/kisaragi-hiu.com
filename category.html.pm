#lang pollen
◊define-meta[type]{page}
◊define-meta[title]{Categories}

◊(current-pagetree "index.ptree")

◊ul{
◊(for/splice ([pagenode (children 'category "index.ptree")])
   (let ([uri (abs-local (~a pagenode))]
         [title (select-from-metas 'title pagenode)])
      `(li ,(link uri title))))
}
