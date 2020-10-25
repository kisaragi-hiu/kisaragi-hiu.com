#lang pollen
◊(require racket/path "../rkt/feed.rkt")
◊(current-pagetree "../index.ptree")
◊(children-to-atom-entries 'blog (current-pagetree))
