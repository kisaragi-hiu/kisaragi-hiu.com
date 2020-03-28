#lang at-exp racket
;; See page-generation.md for more information.

(require "rkt/category.rkt")

(provide generate-category-page
         generate-tag-page)

;; in-category? etc. aren't being run in this module, so no need to
;; require them
(define (generate-category-page category)
  @~a{
      #lang pollen
      ◊(require threading)
      ◊(current-pagetree "../index.ptree")

      ◊define-meta[title]{@(category-display category)}
      ◊define-meta[type]{category-index}

      ◊h1{Category: @(category-display category)}

      ◊(~> (children 'blog (current-pagetree))
        (filter
         (curryr in-category? "@|category|")
         _)
        (map index-item _)
        (txexpr 'div '([class "index"]) _))
      })

(define (generate-tag-page tag)
  @~a{
      #lang pollen
      ◊(require threading)
      ◊(current-pagetree "../index.ptree")

      ◊define-meta[title]{@|tag|}
      ◊define-meta[type]{tag-index}

      ◊(~> (children 'blog (current-pagetree))
        (filter
         (curryr has-tag? "@|tag|")
         _)
        (map index-item _)
        (txexpr 'div '([class "index"]) _))
      })
