#lang racket
(require threading
         txexpr
         pollen/core
         pollen/template
         pollen/decode
         pollen/pagetree
         pollen/tag
         pollen/unstable/pygments
         "widgets.rkt"
         (for-syntax racket/string
                     threading))

(provide (all-defined-out)
         highlight
         (all-from-out "widgets.rkt"
                       pollen/pagetree))

(define author "Kisaragi Hiu")
(define site-prefix "/")
(define site-title "Kisaragi Hiu")
(define site-host "https://kisaragi-hiu.com")
(define (local . rest) (apply ~a site-prefix rest)) ; append local site prefix
(define (global . rest) (apply ~a site-host rest)) ; append global site prefix
(define (extract-xexpr-strings xexpr)
  (if (list? xexpr)
      (filter string? (flatten xexpr))
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

(define/contract (children-to-index p [pagetree (current-pagetree)])
  (->* (pagenodeish?) ([or/c pagetree? pagenodeish?])
       txexpr?)
  (txexpr 'div '([class "index"])
          (map index-item (children p pagetree))))

(define (index-item pagenode #:class [class ""])
  (define uri (local (~a pagenode)))
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
    (p ([class "date-and-category"])
     ,(~a (or date "")
          (if category (~a ", " category) "")
          (if tags (~a " :: " tags) "")))))

