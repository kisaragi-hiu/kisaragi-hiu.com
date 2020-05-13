#lang rackjure
(require xml
         pollen/tag
         pollen/setup
         pollen/template/html
         pollen/core
         pollen/pagetree
         txexpr
         "download.rkt"
         "path.rkt"
         "post.rkt"
         (for-syntax threading))

(provide (all-defined-out))

;;; widgets

(define (index-item pagenode #:class [class ""] #:year? [include-year? #t])
  (define uri (abs-local (~a pagenode)))
  (define date     (select-from-metas 'date pagenode))
  (define title    (select-from-metas 'title pagenode))
  (define category (select-from-metas 'category pagenode))
  (define tags     (select-from-metas 'tags pagenode))
  (unless title
    (error pagenode "title is mandatory"))
  `(div ([class "index-item"])
    ,@(if date
          `((p ([class "date"])
             ,(~> (or (and include-year? (substring date 0 10))
                      (substring date 5 10))
                  (string-replace "-" "/"))))
          empty)
    ,(if category
         `(p ([class "category"])
           (a ([href ,(abs-local (~> (string-downcase category)
                                     (string-replace _ " " "-")
                                     (format "category/~a.html" _)))])
            ,(format "#~a" category)))
         "")
    (h2 ([class "title"])
     (a ([href ,uri]
         [class "text-primary"])
      ,title))))

;; create a widget that is a listing of entries
;; entries are all ptree nodes, ie. output paths as symbols
(define (index entries)
  (let ((entries (sort entries post-date>?)))
    `(div ((class "index"))
      ,@(for/list ((year (remove-duplicates (map post-year entries))))
          `(div ((class "index-year"))
            ,(heading (number->string year))
            (div ((class "index"))
             ,@(~>> entries
                    (filter (curryr post-year=? year))
                    (map (lambda (entry) (index-item entry #:year? #f))))))))))

;; Title in page.
(define (post-heading pagenode)
  (let ((title (select-from-metas 'title pagenode))
        (category (post-category pagenode))
        (date (post-date pagenode)))
    `(div ([class "post-heading"])
      ,(if date
           `(span ([class "date"])
             ,(~> (substring date 0 10)
                  (string-replace "-" "/")))
           "")
      (h2 ([class "title"]) ,title))))

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

(define (make-heading-widget type)
  (lambda (#:id [id #f] . elements)
    `(,type ([id ,(~a (or id (gensym)))]) ,@elements)))

(define h1 (make-heading-widget 'h1))
(define h2 (make-heading-widget 'h2))
(define h3 (make-heading-widget 'h3))
(define heading h1)
(define subheading h2)
(define subsubheading h3)

(define (melpa-badge pkg)
  `(a ([href ,(format "https://melpa.org/#/~a" pkg)]
       [class "badge"])
    (img ([alt "MELPA"]
          [src ,(format "https://melpa.org/packages/~a-badge.svg" pkg)]))))

(define (project url title #:title2 [title2 #f] . description)
  `(div ([class "project"])
    (h2
     (a ([class "project-title"]
         [href ,url])
      ,title)
     ,@(if title2 `(,title2) '()))
    ,@description))

(define (strike . text)
  `(s ,@text))

(define (update timestamp . text)
  `(em (strong "Edit " ,timestamp) ": " ,@text))

(define (update-block timestamp . text)
  `(blockquote () "Edit " ,timestamp ": " ,@text))

(define edit update)
(define edit-block update-block)

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

(define (image src [caption #f]
               #:width [width #f] #:max-height [max-height #f]
               #:class [class ""])
  (define image-style "max-width:100%;")
  (when width
    (set! image-style (~a image-style "width:" width ";")))
  (when max-height
    (set! image-style (~a image-style "max-height:" max-height ";")))
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

(define (video/gif-esque path
                         #:controls? [controls? #f]
                         #:width [width #f]
                         #:max-height [max-height #f]
                         . caption)
  ;; ignore caption for now
  (define style "max-width:100%;")
  (when width
    (set! style (~a style "width:" width ";")))
  (when max-height
    (set! style (~a style "max-height:" max-height ";")))
  (let ([result `(video ([autoplay "autoplay"]
                         [style ,style]
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

;; downloads icons from icongr.am at "compile" time
;; (icon name [#:arg value] ...)
(define icon
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (let* ([arguments ""]
            [name (first args)]
            [local-path (format "static/external/icons/~a.svg" name)])
       ;; set up url arguments
       (map (lambda (kw arg)
              (set! arguments (~a arguments (format "~a=~a" kw arg))))
            kws kw-args)
       ;; arguments not touched if kws and kw-args are empty
       (unless (string=? arguments "")
         (set! arguments (~a "?" arguments)))
       ;; download the icon
       (download (format "https://icongr.am/feather/~a.svg~a" name arguments)
                 (build-path (current-project-root) local-path))
       ;; return the image pointing at the icon
       `(img ([src ,(abs-local local-path)]
              [alt ,name]))))))

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
(define-link niconico "https://www.nicovideo.jp/")
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

(define (envvar . exprs)
  `(span "$" ,@exprs))

(define (path . exprs)
  `(code ,@exprs))

(define (tldr . exprs)
  `(div
    (span (strong "TL;DR: ") ,@exprs)
    (hr)))

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
