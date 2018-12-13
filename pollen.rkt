#lang racket
(require threading
         txexpr
         pollen/core
         pollen/template
         pollen/decode
         pollen/pagetree
         pollen/tag
         pollen/unstable/pygments
         racket/function
         "widgets.rkt"
         "date.rkt"
         (for-syntax racket/string
                     threading))

(provide (all-defined-out)
         highlight
         (all-from-out "widgets.rkt"
                       pollen/pagetree
                       pollen/template
                       racket/function
                       txexpr))

(define author "Kisaragi Hiu")
(define site-prefix "/")
(define site-title "Kisaragi Hiu")
(define site-host "https://kisaragi-hiu.com/")
(define (abs-local . rest) (apply ~a site-prefix rest)) ; append local site prefix
(define (abs-global . rest) (apply ~a site-host rest)) ; append global site prefix
(define (extract-xexpr-strings xexpr)
  (if (list? xexpr)
      (string-join (filter string? (flatten xexpr)) "")
      xexpr))

(define (root . elements)
  (txexpr 'root '() (decode-elements
                     elements
                     #:txexpr-elements-proc decode-paragraphs
                     #:exclude-tags '(pre))))

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

(define/contract (children-to-index p [pagetree (current-pagetree)])
  (->* (pagenodeish?) ([or/c pagetree? pagenodeish?])
       txexpr?)
  (txexpr 'div '([class "index"])
          (map index-item (children p pagetree))))

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
          (iso8601-date->year-and-month-str date)
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
    ,(if prev (navbutton prev "<") "")
    ,(if next (navbutton next "" ">") "")))

(define (previous-and-next pagenode)
  (page-navigation #:extra-classes "prev-next-all"
                   (previous pagenode)
                   (next pagenode)))

(define (previous-and-next-same-category pagenode)
  ;; Fixme: This seems to take forever during first run in a REPL.
  ;; Is this a serious issue?
  (define posts-in-this-category
    (filter (curryr in-category? (select-from-metas 'category pagenode))
            (children 'blog)))
  ;; take everything from the start until the function is #f
  ;; ie. until we hit the input pagenode
  (define before-this
    (takef posts-in-this-category
           (lambda (p) (not (eq? pagenode p)))))
  ;; same, but from the end
  (define after-this
    (takef-right posts-in-this-category
                 (lambda (p) (not (eq? pagenode p)))))
  (page-navigation #:extra-classes "prev-next-category"
                   (last before-this)
                   (first after-this)))
