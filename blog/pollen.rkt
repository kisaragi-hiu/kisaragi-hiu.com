#lang racket/base
(require (for-syntax racket/base)
         pollen/core
         pollen/decode
         pollen/tag
         pollen/template
         pollen/unstable/pygments
         racket/date
         racket/dict
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/string
         threading
         txexpr
         "widgets.rkt"
         "define-txexpr.rkt"
         "translate.rkt"
         (only-in xml
                  string->xexpr))


(provide (all-defined-out)
         (all-from-out pollen/template
                       "widgets.rkt")
         (rename-out [translate !])
         (rename-out [translate-inline $]))

(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (append '(subsection subsubsection label img pre) default-block-tags)))

(define import file->string)

(define/txexpr (diff-old . elements) `(span ([class "diff-old"]) ,@elements))
(define/txexpr (diff-new . elements) `(span ([class "diff-new"]) ,@elements))

;; I'm naming the arguments so calls would be a little more readable
(define (article-header #:date date ; :: string? ex: "@|date|"
                        #:tags tags ; :: string? ex: "@|tags|"
                        #:category category ; string? ex: "@|category|"
                        #:title title ; :: string? ex: "@|title|"
                        #:uri uri ; :: string? ex: "@|full-uri|"
                        #:class class) ; :: string? ex: "post-header"
  (->html
   `(header ([class ,class])
            (h2 ([class "title mb-0"])
                (a ([href ,uri]
                    [class "text-primary"])
                   ,title))
            (p ([class "date-and-category"])
               (span ,date)
               ,(string-append "@(when " category " \", \")")
               ,category
               ,(string-append "@(when " tags " \" :: \")")
               ,tags))))

(define (quotation . lines)
  ;; lines :: (listof string?)
  (~> (string-join lines)
      (string-replace _ "\n" "\n> ")
      (string-append "> " _) ; first line has no ">" from above. add it
      (newline-decode _)))

(define (quotation-html . lines)
  ;; lines :: (listof string?)
  (~> (apply newline-decode lines)
      (string-append "<blockquote><p>" _ "</p></blockquote>")))

(define (highlight language . stuff)
  (define lang-string
    (if (symbol? language)
      (symbol->string language)
      language))
  (string-append "```" lang-string "\n"
                 (string-join stuff "")
                 "\n```\n"))

(define pagebreak (->html '(div ([class "page-break"]))))

(define (filepath . strs)
  (string-join strs #:before-first "`" #:after-last "`"))

(define/txexpr (year . text)
  `(p ([class "year-in-page"]) ,@text))

(define ie "i.e.")

(define (font-awesome fa-icon #:aria [hidden #t] #:color [color #f] #:size [size #f])
  (define options (if (or color size) "?" ""))
  (when color (set! options (string-append options "color=" color)))
  (when size (set! options (string-append options "size=" size)))
  `(img ([src ,(string-append "https://icongr.am/fontawesome/" fa-icon ".svg" options)]
         [alt ,fa-icon])))
