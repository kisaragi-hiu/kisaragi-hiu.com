#lang racket/base

(require pollen/cache
         pollen/core
         pollen/pagetree
         pollen/template
         racket/format
         racket/list
         racket/path
         racket/string
         threading
         "config.rkt"
         "misc.rkt"
         "path.rkt")

(provide children-to-atom-entries
         atom-entry)

(define (atom-entry pagenode [pagetree (current-pagetree)])
  (define full-uri (abs-global (~a pagenode)))
  (define source
    (~>
     (map path->string (directory-list #:build? #t (path-only (~a "../" pagenode))))
     (filter (λ (p) (and
                      (string-contains? p (~a pagenode))
                      (not (string-suffix? p (~a pagenode)))))
             _)
     first
     path->complete-path
     normalize-path))
  (define date        (select-from-metas 'date source))
  (define title       (select-from-metas 'title source))
  (define category    (select-from-metas 'category source))
  (define tags        (select-from-metas 'tags source))
  (define this-author (select-from-metas 'author source))
  `(entry
    (title ([type "text"]) ,(extract-xexpr-strings title))
    (id ,(urn (~a pagenode)))
    (published ,(ensure-timezone date))
    (updated ,(ensure-timezone date))
    (link ([rel "alternate"] [href ,(~a full-uri "?utm_source=all&utm_medium=Atom")]))
    (author (name ,(or this-author (conf 'author))))
    (content ([type "html"])
     ,(~> (cached-doc source)
          (->html #:splice #t)))))

(define (children-to-atom-entries p [pagetree (current-pagetree)])
  (map atom-entry (children p pagetree)))
