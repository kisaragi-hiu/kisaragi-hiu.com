#lang rackjure
(require xml
         pollen/tag
         pollen/template/html
         txexpr)

(provide (all-defined-out))

;;; widgets

(define (link url [text url]
              #:class [class #f]
              #:target [target #f])
  (define result `(a ([href ,url]) ,text))
  (when class (set! result (attr-set result 'class class)))
  (when target (set! result (attr-set result 'target target)))
  result)

(define (dropdown #:button-id button-id
                  #:button-extra-classes button-extra-classes
                  #:button-label button-label
                  .
                  elements)
  `(div ([class "dropdown"])
    (a ([class ,(~a "btn dropdown-toggle " button-extra-classes)]
        [href "#"]
        [role "button"]
        [id ,button-id]
        [data-toggle "dropdown"]
        [aria-haspopup "true"]
        [aria-expanded "false"])
     ,button-label)
    ;; add attrs to each element, then put them in a dropdown-menu div
    ,(~> (map
          (lambda (tx)
            (attr-set* tx 'class "dropdown-item" 'aria-labelledby button-id))
          elements)
         (txexpr 'div '([class "dropdown-menu"]) _))))

(define heading (default-tag-function 'h1))
(define subheading (default-tag-function 'h2))

(define (project url title description)
  `(li ([class "project"])
    (a ([class "project-link"]
        [href ,url])
     (p ([class "project-title"])
      ,title
      nbsp)
     (p ([class "project-description"])
      ,description))))

(define (collapse #:button-classes button-classes
                  #:button-label button-label
                  #:div-id div-id
                  #:div-extra-classes [div-extra-classes ""]
                  .
                  elements)
  `(div (div (a ([class ,button-classes]
                 [data-toggle "collapse"]
                 [href ,(~a "#" div-id)]
                 [role "button"]
                 [aria-expanded "false"]
                 [aria-controls ,div-id])
              ,button-label))
    (div ([class ,(~a "collapse " div-extra-classes)]
          [id ,div-id])
     ,@elements)))

(define (collapse-button #:button-class [button-class ""]
                         #:div-id [div-id ""]
                         .
                         elements)
  `(div (a ([class ,button-class]
            [data-toggle "collapse"]
            [href ,(~a "#" div-id)]
            [role "button"]
            [aria-expanded "false"]
            [aria-controls ,div-id])
         ,@elements)))

(define (strike . text)
  `(s ,@text))

(define (stylized-item text)
  (txexpr* 'div '()
           `(span ([style "font-weight: 700;"]) ,text)
           `(hr ([style
                  ,(string-append "height: 1px;"
                                  "width: 6rem;"
                                  "margin: 0.3rem auto 1.2rem;"
                                  "text-align: left;"
                                  "margin-right: 100%;"
                                  "background-color: #444;")]))))

(define (rant . text)
  `(span ([style "color: #777;"]) "(" ,@text ")"))

(define (image src [caption #f] #:width [width #f] #:class [class ""])
  (define image-style "max-width:100%;")
  (when width
    (set! image-style (~a image-style "width:" width ";")))
  `(div (img ([src ,src] [style ,image-style] [class ,class]))
    ,(if caption
         `(p ([class "image-caption"]) ,caption)
         "")))

(define (R text ruby) `(ruby ,text (rt ,ruby)))

(define (table . elements)
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

(define (video/gif-esque path #:controls? [controls? #f] . caption)
  ;; ignore caption for now
  (let ([result `(video ([autoplay "autoplay"]
                         [style "max-width: 100%;"]
                         [muted "muted"]
                         [loop "loop"]
                         [src ,path]))])
    (if controls?
        (attr-set result 'controls "")
        result)))

(define (kbd . elements)
  `(kbd ,@elements))

(define (youtube/embed video-id
                       #:class [class "ytembed-default"]
                       #:style [style ""])
  `(p ([class ,class]
       [style ,style])
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
                    "https://www.youtube.com/embed/"
                    video-id
                    "?autoplay=0"
                    "&origin="
                    "https://kisaragi-hiu.com")]
             [frameborder "0"]))))

(define (youtube/image-link video-id #:class [class ""])
  (image/link #:class class
              (~a "https://youtu.be/" video-id)
              (format
               "https://img.youtube.com/vi/~a/mqdefault.jpg"
               video-id)))

;;; Link functions

(define (image/link url src [caption #f] #:class [class ""])
  (define img-link
    `(div
      ([class ,(format "image-link ~a" class)])
      ,(link url
             (image src))))
  (when (string? caption)
    (set! img-link
          ;; (append '(a) '(b)) is '(a b).
          ;; We want '(a (b)), so we put 'p in another list.
          (append img-link `((p ([class "image-caption"]) ,caption)))))
  img-link)

(define (link/date url date . text)
  `(p ,(string-append date " ")
    ,(link url (string-join text))))

(define-syntax (define-link stx)
  (syntax-case stx ()
    [(_ linkname url-prefix)
     #'(begin
         (define (linkname suburl [text suburl] #:class [class ""])
           (link (string-append url-prefix suburl)
                 text
                 #:class class)))]))

(define (font-awesome fa-icon #:aria [hidden #t] #:color [color #f] #:size [size #f])
  (define options (if (or color size) "?" ""))
  (when color (set! options (string-append options "color=" color)))
  (when size (set! options (string-append options "size=" size)))
  `(img ([src ,(string-append "https://icongr.am/fontawesome/" fa-icon ".svg" options)]
         [alt ,fa-icon])))

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
