#lang pollen
◊define-meta[type]{page}
◊define-meta[title]{Categories}

◊(current-pagetree "index.ptree")

◊heading{Categories}

◊(for/splice ([category (children 'category "index.ptree")])
   (index-item category))
