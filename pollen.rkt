#lang racket
(require threading
         txexpr
         pollen/core
         pollen/cache
         pollen/template
         pollen/decode
         pollen/pagetree
         pollen/render
         pollen/tag
         pollen/unstable/pygments
         racket/function
         racket/list
         "widgets.rkt"
         "date.rkt"
         "path.rkt"
         (for-syntax racket/string
                     threading))

(provide (all-defined-out)
         highlight
         (all-from-out
          "widgets.rkt"
          "path.rkt"
          pollen/pagetree
          pollen/template
          racket/function
          racket/list
          txexpr))

(module+ setup
  (require syntax/modresolve)
  (provide (all-defined-out))
  (define cache-watchlist
    (map resolve-module-path
         '("date.rkt"
           "download.rkt"
           "path.rkt"
           "widgets.rkt"))))

(define author "Kisaragi Hiu")
(define site-title "Kisaragi Hiu")

(define (extract-xexpr-strings xexpr)
  (if (list? xexpr)
      (string-join (filter string? (flatten xexpr)) "")
      xexpr))

(define (root . elements)
  (decode (txexpr 'root '() elements)
          #:txexpr-elements-proc decode-paragraphs
          #:exclude-tags '(style pre script)
          #:exclude-attrs '((class "tweet"))))

(define-syntax (->2to-define stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax
       ([new-name
         (datum->syntax
          #'name
          (~> (syntax->datum #'name)
              symbol->string
              (string-replace _ "->" "-to-")
              (string-replace _ #rx"^-" "")
              string->symbol))])
       #'(define new-name name))]))

(->2to-define ->html)

(define (in-category? pagenode category)
  (define cat (select-from-metas 'category pagenode))
  (and (string? cat)
       (string-ci=? cat category)))

(define (has-tag? pagenode tag)
  ;; select-from-metas is #f or txexpr only.
  ;; tags need to be some sort of list. use (select)?
  ;; or should tags be a comma-seperated string?
  (define this-tags (select-from-metas 'tags pagenode))
  (and (not (empty? this-tags))
       (member tag this-tags)))

(define/contract (pagetree-to-index pagetree)
  (-> pagetree? txexpr?)
  (txexpr 'div '([class "index"])
          (map index-item (pagetree->list pagetree))))

(define/contract (post-year pagenode)
  (-> (or/c pagenode? pagenodeish?)
      number?)
  (string->number
   (substring (select-from-metas 'date pagenode)
              0 4)))

(define/contract (post-year=? pagenode year)
  (-> (or/c pagenode? pagenodeish?) number?
      boolean?)
  (= year (post-year pagenode)))

(define (ensure-timezone str)
  ;; default to +8 timezone
  (if (regexp-match? #rx"\\+|Z" str)
      str
      (~a str "+08:00")))

(define (atom-entry pagenode [pagetree (current-pagetree)])
  (define full-uri (abs-global (~a pagenode)))
  (define date        (select-from-metas 'date pagenode))
  (define title       (select-from-metas 'title pagenode))
  (define category    (select-from-metas 'category pagenode))
  (define tags        (select-from-metas 'tags pagenode))
  (define this-author (select-from-metas 'author pagenode))
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
  `(entry
    (title ([type "text"]) ,(extract-xexpr-strings title))
    (id ,(urn (~a pagenode)))
    (published ,(ensure-timezone date))
    (updated ,(ensure-timezone date))
    (link ([rel "alternate"] [href ,(~a full-uri "?utm_source=all&utm_medium=Atom")]))
    (author (name ,(or this-author author)))
    (content ([type "html"])
     ,(~> (cached-doc source)
          (->html #:splice #t)))))

(define (children-to-atom-entries p [pagetree (current-pagetree)])
  (map atom-entry (children p pagetree)))

(define (index-item pagenode #:class [class ""])
  (define uri (abs-local (~a pagenode)))
  (define date     (select-from-metas 'date pagenode))
  (define title    (select-from-metas 'title pagenode))
  (define category (select-from-metas 'category pagenode))
  (define tags     (select-from-metas 'tags pagenode))
  (unless title
    (error pagenode "title is mandatory"))
  `(header ([class ,class])
    (h2 ([class "title mb-0"])
     (a ([href ,uri]
         [class "text-primary"])
      ,title))
    (p ([class "date"])
     ,(if date
          (string-replace (substring date 0 10) "-" "/")
          ""))
    ,(if category
         `(p ([class "category"])
           "C: "
           (a ([href ,(abs-local "category/"
                                 (string-replace
                                  (string-downcase category)
                                  " "
                                  "-")
                                 ".html")])
            ,(string-titlecase category)))
         "")
    ,(if (list? tags)
         `(p ([class "tags"])
           "T: "
           ,(~> (for/list ((tag tags))
                  `(a ([href ,(abs-local "tags/" tag)])
                    ,tag))
                (string-join _ ", ")))
         "")))

;; get type of current document
(define (document-type metas)
  (or (select-from-metas 'type metas)
      "post"))

;; before and after are labels, so they must be strings
(define (navbutton pagenode [before ""] [after ""])
  `(a ([href ,(abs-local (~a pagenode))])
    ,before
    ,(select-from-metas 'title pagenode)
    ,after))

(define (page-navigation prev next #:extra-classes [extra-classes ""])
  `(div ([class ,(~a "page-navigation " extra-classes)])
    ,(if prev
         (navbutton prev "< ")
         `(span ([class "disabled"]) "< No newer article"))
    ,(if next
         (navbutton next "> " "")
         `(span ([class "disabled"]) "> No older article"))))

(define (previous-and-next pagenode)
  (parameterize ([current-pagetree `(root ,@(siblings pagenode))])
    (page-navigation (previous pagenode)
                     (next pagenode))))

(define (previous-and-next-same-category pagenode)
  (parameterize ([current-pagetree `(root ,@(siblings pagenode))])
    (define previous-page (and~> (previous* pagenode) last))
    (define next-page (and~> (next* pagenode) first))

    (page-navigation #:extra-classes "prev-next-category"
                     previous-page
                     next-page)))

(define (toc pagenode)
  ;; as this depends on tagging headings with ids, this won't work with pmd files.
  (define doc (get-doc pagenode))
  (define (toc-item tx level)
    (txexpr 'a `([href ,(~a "#" (attr-ref tx 'id))]
                 [class ,(~a "toc-" level)])
            (get-elements tx)))
  `(@
    (h1 ([id "toc-title"])
     "Table of Contents")
    (div ([class "toc"])
     ,@(filter
        txexpr?
        (for/list ([elem doc])
          (case (and (txexpr? elem)
                     (car elem))
            [(h1)
             (toc-item elem 'h1)]
            [(h2)
             (toc-item elem 'h2)]
            [(h3)
             (toc-item elem 'h3)]
            [(h4)
             (toc-item elem 'h4)]
            [(h5)
             (toc-item elem 'h5)]
            [(h6)
             (toc-item elem 'h6)]
            [else #f]))))))
