#lang pollen
◊(require racket/path)
◊(current-pagetree "../index.ptree")
◊(children-to-atom-entries 'blog (normalize-path (path->complete-path "../index.ptree")))
