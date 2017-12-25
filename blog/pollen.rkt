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
         txexpr)

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
                  [(regexp-match #rx"\n\n+" x) "<br>\n\n"]
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

(define (link url #:class [class ""]
              #:target [target "_blank"]
              #:txexpr? [txexpr? #f] . text)
  (let ([wrapper (if txexpr? identity ->html)])
    (wrapper
     `(a ([href ,url]
          [target ,target]
          [class ,class])
         ,@text))))

(define (image/link url src caption)
  (->html `(div
            ,(link #:txexpr? #t url
                   (image src #:txexpr? #t))
            (p ([class "image-caption"]) ,caption))))

(define (link/date url date . text)
  (->html
   `(p ,(string-append date " ")
       ,(link url (string-join text)))))

(define (L site sub text #:class [class ""])
  (~> (hash 'github "https://github.com/"
            'youtube "https://youtube.com/"
            'pixiv "https://pixiv.net/"
            'niconico "http://www.nicovideo.jp/"
            'osuwiki "http://osu.ppy.sh/help/wiki/"
            'transifex "https://www.transifex.com/user/profile/"
            'noichigo "https://www.no-ichigo.jp/read/book/book_id/"
            'twitter "https://twitter.com/")
      (dict-ref _ site)
      (string-append _ sub)
      (link _ #:class class text)))

; wrapper around L
(define (twitter sub text)
  (L 'twitter
     sub
     text))
(define (github sub text)
  (L 'github
     sub
     text))
(define (youtube sub text)
  (L 'youtube
     sub
     text
     #:class "youtube"))
(define (pixiv sub text)
  (L 'pixiv
     sub
     text
     #:class "pixiv"))
(define (niconico sub text)
  (L 'niconico
     sub
     text
     #:class "niconico"))
(define (osuwiki sub text)
  (L 'osuwiki
     sub
     text
     #:class "osuwiki"))
(define (transifex sub text)
  (L 'transifex
     sub
     text))
(define (noichigo sub text)
  (L 'noichigo
     sub
     text))

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

(define (google-adsense id)
  (string-append "<script async src=\"//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>
<script>
  (adsbygoogle = window.adsbygoogle || []).push({
    google_ad_client: " id ",
    enable_page_level_ads: true
  });
</script>"))

