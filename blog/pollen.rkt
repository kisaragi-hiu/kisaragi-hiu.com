#lang at-exp racket/base
(require pollen/core
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
         (for-syntax racket/base))

(provide (all-defined-out)
         (all-from-out pollen/template))

(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (append '(subsection subsubsection label img pre) default-block-tags)))

(define import file->string)

(define (diff-old . elements) (->html `(span ([class "diff-old"]) ,@elements)))
(define (diff-new . elements) (->html `(span ([class "diff-new"]) ,@elements)))

(define (strike . text)
  (->html `(s ,@text)))

(define (image src [caption #f] #:width [width #f] #:txexpr? [txexpr? #f])
  (let ([wrapper (if txexpr? identity ->html)])
    ;; "Backwards compatibility"
    (wrapper `(div (img ([src ,src]))
                   ,(if caption
                        `(p ([class "image-caption"]) ,caption)
                        "")))))

(define (R text ruby) `(ruby ,text (rt ,ruby)))

(define (table . elements)
  (->html `(table ,@elements)))

(define (newline-decode . elements)
  (string-join
   (map (λ (x) (cond
                  [(not (string? x)) x]
                  [(regexp-match #rx"\n\n+" x) "\n\n"]
                  [(regexp-match #rx">\n+" x) ">\n"]
                  [(regexp-match #rx"\n" x) "<br>\n"]
                  [else x]))
        elements)
   ""))

(define pagebreak (->html '(div ([class "page-break"]))))

(define (year . text)
  (->html `(p ([class "year-in-page"]) ,@text)))

(define ie "i.e.")

(define (font-awesome fa-icon
                      #:aria [hidden #t]
                      #:txexpr [return-txexpr #f])
  (~>
   `(i ([class ,(string-append "fa "
                               fa-icon)]
        [aria-hidden "true"]))
   ((if return-txexpr
        identity
        ->html) _)))

#| link functions |#

(define (link url [text url]
              #:class [class ""]
              #:target [target "_blank"]
              #:txexpr? [txexpr? #f])
  (let ([wrapper (if txexpr? identity ->html)])
    (wrapper
     `(a ([href ,url]
          [target ,target]
          [class ,class])
         ,text))))

(define (image/link url src caption)
  (->html `(div
            ,(link #:txexpr? #t url
                   (image src #:txexpr? #t))
            (p ([class "image-caption"]) ,caption))))

(define (link/date url date . text)
  (->html
   `(p ,(string-append date " ")
       ,(link url (string-join text)))))

(define-syntax (define-link stx)
  (syntax-case stx ()
    [(_ linkname url-prefix)
     #'(begin
         (define (linkname suburl [text suburl] #:class [class ""])
           (link (string-append url-prefix suburl)
                 text
                 #:class class)))]))

(define-link github "https://github.com/")
(define-link twitter "https://twitter.com/")
(define-link youtube "https://youtube.com/")
(define-link pixiv "https://pixiv.net/")

(define-link niconico "http://www.nicovideo.jp/")
(define-link osuwiki "http://osu.ppy.sh/help/wiki/")
(define-link transifex "https://www.transifex.com/user/profile/")
(define-link noichigo "https://www.no-ichigo.jp/read/book/book_id/")

(define site-url "http://kisaragi-hiu.com")
(define (youtube/embed video-id)
  (->html
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
               [frameborder "0"])))))

(define (google-adsense/page-level id)
  (string-append
    "<script async src=\"//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>\n"
    "<script>\n"
    "(adsbygoogle = window.adsbygoogle || []).push({\n"
    "  google_ad_client: " id ",\n"
    "  enable_page_level_ads: true\n"
    "});\n"
    "</script>"))

(define google-adsense/banner
  (->html
   '((script ([async "async"]
              [src "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"]))
     (ins ([class "adsbygoogle"]
           [data-ad-client "ca-pub-6215394828182929"]
           [data-ad-slot "7498976067"]
           [style "display:inline-block;width:728px;height:90px"]))
     (script "(adsbygoogle = window.adsbygoogle || []).push({});"))))

(define google-adsense/in-article
  (->html
   '((script ([async "async"]
              [src "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"]))
     (ins ([class "adsbygoogle"]
           [style "display:block;text-align:center"]
           [data-ad-layout "in-article"]
           [data-ad-format "fluid"]
           [data-ad-client "ca-pub-6215394828182929"]
           [data-ad-slot "5614709443"]))
     (script "(adsbygoogle = window.adsbygoogle || []).push({});"))))

(define google-adsense/responsive
  (->html
   '((script ([async "async"]
              [src "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"]))
     (ins ([class "adsbygoogle"]
           [style "display:block"]
           [data-ad-client "ca-pub-6215394828182929"]
           [data-ad-slot "5733348692"]
           [data-ad-format "auto"]))
     (script "(adsbygoogle = window.adsbygoogle || []).push({});"))))
