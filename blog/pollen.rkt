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
         (only-in xml
                  string->xexpr))


(provide (all-defined-out)
         (all-from-out pollen/template
                       "widgets.rkt"))

(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (append '(subsection subsubsection label img pre) default-block-tags)))

(define current-return-txexpr? (make-parameter #f))

(define-syntax (define/txexpr stx)
  (syntax-case stx ()
    [(_ (name args ... . rest) body ... last-body)
     #'(begin
         (define (name #:return-txexpr? [return-txexpr? (current-return-txexpr?)] args ... . rest)
           body ...
           ((if return-txexpr? identity ->html) last-body)))]))

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
            (p ([class "title mb-0"])
               (a ([href ,uri]
                   [class "text-primary"])
                  ,title))
            (p ([class "date-and-category"])
               (span ,date)
               ,(string-append "@(when " category " \", \")")
               ,category
               ,(string-append "@(when " tags " \" :: \")")
               ,tags))))

(define/txexpr (strike . text)
  `(s ,@text))

(define/txexpr (stylized-item text)
  (txexpr* 'div '()
           `(span ([style "font-weight: 700;"]) ,text)
           `(hr ([style
                     ,(string-append "height: 1px;"
                                     "width: 6rem;"
                                     "margin: 0.3rem auto 1.2rem;"
                                     "text-align: left;"
                                     "margin-right: 100%;"
                                     "background-color: #444;")]))))

(define/txexpr (rant . text)
  `(span ([style "color: #777;"]) "(" ,@text ")"))

(define/txexpr (image src [caption #f] #:width [width #f])
  (define image-style "max-width:100%;")
  (when width
    (set! image-style (~a image-style "width:" width ";")))
  `(div (img ([src ,src] [style ,image-style]))
        ,(if caption
             `(p ([class "image-caption"]) ,caption)
             "")))

(define/txexpr (R text ruby) `(ruby ,text (rt ,ruby)))

(define/txexpr (table . elements)
  `(table ,@elements))

(define (newline-decode . elements)
  (string-join
   (map (λ (x) (cond
                  [(not (string? x)) x]
                  [(regexp-match #rx"\n\n+" x)
                   (string-replace x #rx"\n\n+" "\n\n")]
                  [(regexp-match #rx">\n+" x) ">\n"
                   (string-replace x #rx">\n+" ">\n")]
                  [(regexp-match #rx"\n" x) "<br>\n"
                   (string-replace x #rx"\n" "<br>\n")]
                  [else x]))
        elements)
   ""))

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

#| link functions |#

(define/txexpr (link url [text url]
                     #:class [class ""]
                     #:target [target "_self"])
  `(a ([href ,url]
       [target ,target]
       [class ,class])
      ,text))

(define/txexpr (image/link url src caption)
  `(div
     ,(link #:return-txexpr? #t url
            (image src #:return-txexpr? #t))
     (p ([class "image-caption"]) ,caption)))

(define/txexpr (link/date url date . text)
  `(p ,(string-append date " ")
      ,(link #:return-txexpr? #t url (string-join text))))

(define-syntax (define-link stx)
  (syntax-case stx ()
    [(_ linkname url-prefix)
     #'(begin
         (define/txexpr (linkname suburl [text suburl] #:class [class ""])
           (link (string-append url-prefix suburl)
                 text
                 #:class class
                 #:return-txexpr? #t)))]))

(define-link github "https://github.com/")
(define-link gitlab "https://gitlab.com/")
(define-link twitter "https://twitter.com/")
(define-link youtube "https://youtube.com/")
(define-link pixiv "https://pixiv.net/")
(define-link niconico "http://www.nicovideo.jp/")
(define-link osuwiki "http://osu.ppy.sh/help/wiki/")
(define-link transifex "https://www.transifex.com/user/profile/")
(define-link noichigo "https://www.no-ichigo.jp/read/book/book_id/")
(define-link site-crossref "https://kisaragi-hiu.com/")

(define site-url "http://kisaragi-hiu.com")

(define/txexpr (video/gif-esque path #:controls? [controls? #f] . caption)
  ;; ignore caption for now
  (let ([result `(video ([autoplay "autoplay"]
                         [style "max-width: 100%;"]
                         [muted "muted"]
                         [loop "loop"]
                         [src ,path]))])
    (if controls?
        (attr-set result 'controls "")
        result)))

(define/txexpr (kbd . elements)
  `(kbd ,@elements))

(define/txexpr (youtube/embed video-id)
  `(div ([style "padding-bottom: 50%;
                position: relative;
                overflow: hidden;"])
        (iframe ([id "ytplayer"]
                 [type "text/html"]
                 [width "640"]
                 [height "360"]
                 [style "position: absolute;
                        top: 0;
                        left: 0;
                        width: 100%;
                        height: 100%;"]
                 [src ,(string-append
                        "http://www.youtube.com/embed/"
                        video-id
                        "?autoplay=0"
                        "&origin="
                        site-url)]
                 [frameborder "0"]))))
