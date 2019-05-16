#lang rackjure
(require xml
         pollen/tag
         pollen/setup
         pollen/template/html
         txexpr
         "download.rkt"
         "path.rkt"
         (for-syntax threading))

(provide (all-defined-out))

;;; widgets

(define (link url
              #:class [class #f]
              #:target [target #f]
              . text)
  ;; text is set to url if not given
  (when (empty? text)
    (set! text (list url)))
  (define result `(a ([href ,url]) ,@text))
  (when class (set! result (attr-set result 'class class)))
  (when target (set! result (attr-set result 'target target)))
  result)

;; TODO: actually implement tabbed stuff
(define (tabbed . language-examples)
  `(div ([class "tabbed"])
        ,@(for/list ([example language-examples])
            `(div
              (span ,(first example) ":")
              ,(second example)))))

(define (h1 . elements)
  `(h1 ([id ,(~a (gensym))]) ,@elements))
(define (h2 . elements)
  `(h2 ([id ,(~a (gensym))]) ,@elements))
(define (h3 . elements)
  `(h3 ([id ,(~a (gensym))]) ,@elements))
(define heading h1)
(define subheading h2)
(define subsubheading h3)

(define (project url title description)
  `(li ([class "project"])
    (a ([class "project-link"]
        [href ,url])
     (p ([class "project-title"])
      ,title
      nbsp)
     (p ([class "project-description"])
      ,description))))

(define (edit date . text)
  `(blockquote () "Edit " ,date ": " ,@text))

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
  `(div ([class "image"])
    (img ([src ,src] [style ,image-style] [class ,class]))
    ,(if caption
         `(p ([class "image-caption"]) ,caption)
         "")))

(define (R text ruby) `(ruby ,text (rt ,ruby)))

(define (table . elements)
  `(table ,@elements))

(define (command . elements) `(code ([class "command"]) ,@elements))

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
         (define (linkname suburl [text #f] #:class [class ""])
           (link (string-append url-prefix suburl)
                 (if text
                     text
                     ;; linkname ends up being a function...
                     (~a (~> (~a linkname)
                             ;; #<procedure:gist> -> procedure:gist
                             (string-trim #px"[#<>]+")
                             ;; procedure:gist -> gist
                             (string-replace "procedure:" ""))
                         ":" suburl))
                 #:class class)))]))

(define (font-awesome fa-icon #:aria [hidden #t] #:color [color #f] #:size [size #f])
  (define options (if (or color size) "?" ""))
  (when color (set! options (string-append options "color=" color)))
  (when size (set! options (string-append options "size=" size)))
  (download (~a "https://icongr.am/fontawesome/" fa-icon ".svg" options)
            (build-path (current-project-root) "static/ext/fa/" fa-icon ".svg"))
  `(img ([src ,(abs-local "static/ext/fa/" fa-icon ".svg")]
         [alt ,fa-icon])))

(define (tweet url
               #:summary summary
               #:author author
               #:profile profile
               #:date date)
  ;; a translation of Twitter's default embed code
  `(div ([class "tweet"])
    (blockquote ([class "twitter-tweet"]
                 [data-lang "ja"])
     (p ([lang "en"]
         [dir "ltr"])
      ,summary)
     "—"
     ,author " (@" ,profile ")"
     (a ([href ,url])
      ,date))
    (script ([async "async"]
             [src "https://platform.twitter.com/widgets.js"]
             [charset "utf-8"]))))

(define-link github "https://github.com/")
(define-link gist "https://gist.github.com/")
(define-link gitlab "https://gitlab.com/")
(define-link twitter "https://twitter.com/")
(define-link youtube "https://youtube.com/")
(define-link pixiv "https://pixiv.net/")
(define-link niconico "http://www.nicovideo.jp/")
(define-link osuwiki "http://osu.ppy.sh/help/wiki/")
(define-link transifex "https://www.transifex.com/user/profile/")
(define-link noichigo "https://www.no-ichigo.jp/read/book/book_id/")
(define-link site-crossref "https://kisaragi-hiu.com/")

(define (emacs-source #:branch [branch #f]
                      #:commit [commit #f]
                      #:file [file #f]
                      #:line [line ""]
                      . content)
  (define arguments "")
  (unless (string=? line "")
    (set! line (~a "#n" line)))
  (when branch
    (set! branch (~a "h=" branch)))
  (when commit
    (set! commit (~a "id=" commit)))
  (set! arguments
    (string-join
     (filter identity (list branch commit)) ; get rid of #f's
     "&" #:before-first "?"))
  (unless file
    (set! file (first content)))
  (link
   (format "https://git.savannah.gnu.org/cgit/emacs.git/tree/~a~a~a"
           file arguments line)
   #:class "emacs-source"
   `(code ,@content)))

(define (tldr . exprs)
  `(div
    (span (strong "TL;DR: ") ,@exprs)
    (hr)))
