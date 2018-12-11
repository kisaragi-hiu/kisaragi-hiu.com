#lang at-exp racket
;; See page-generation.md for more information.

(provide generate-category-page
         generate-tag-page)

(define (generate-category-page category)
  @~a{
      #lang pollen
      ◊(require threading)
      ◊(current-pagetree "../index.ptree")

      ◊define-meta[title]{@|category|}
      ◊define-meta[type]{category-index}

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
