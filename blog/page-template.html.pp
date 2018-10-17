#lang pollen
◊; This file is fed through pollen first.
◊; This `require`s eg. ->html for use as a preprocessor.
◊(require pollen/template)

@; This needs local-require as it's a template
@(local-require (only-in xml string->xexpr)
                txexpr
                json
                threading
                racket/format
                racket/string
                "tags.rkt"
                "content-processing.rkt")

@(define all-tags (tag-string->tags tags-list-items))

@(define tags.json (build-path (find-system-path 'temp-dir) "tags.json"))
@(unless (file-exists? tags.json)
  (define p (open-output-file tags.json))
  (display (jsexpr->string (tags->jsexpr all-tags)) p)
  (close-output-port p))

<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    <title>@|title|</title>
    <meta name="description" content="@|description|">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="author" content="@|author|">
    <meta name="keywords" content="@|keywords|">
    <link rel="icon" href="@|uri-prefix|/favicon.ico">
    <link rel="canonical" href="@|full-uri|">

    @(when rel-next @list{<link rel="next" href="@|rel-next|">})
    @(when rel-prev @list{<link rel="prev" href="@|rel-prev|">})
    ◊; Font
    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Sans|Overpass+Mono|Overpass:400,600">
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">
    <link rel="stylesheet" type="text/css" href="/css/emacs.css">
    <link rel="stylesheet" type="text/css" href="/css/main.css">
    <link rel="alternate" type="application/atom+xml"
          href="@|atom-feed-uri|" title="Atom Feed">
  </head>
  <body>
    <!--[if lte IE 9]>
      ◊; … too aggressive?
      <p>For the love of god, please stop using IE 9. Thanks.</p>
    <![endif]-->
    <a id="top"></a>

    <div class="container">
      ◊; Header
      <header id="header" class="py-2">
        <div class="row flex-nowrap justify-content-between alien-items-center">
          <div id="logo" class="col-4 pt-1">
            <div class="d-flex justify-content-begin">
              <a href="/" target="_self" class="py-2 pr-2"><img src="/images/avatar.png" alt="Kisaragi Hiu"/></a>
            </div>
          </div>
          <div id="social-links" class="col-4 pt-1">
            <div class="d-flex justify-content-center">
              ◊link["@|atom-feed-uri|" #:class "p-2"]{
                ◊font-awesome["rss" #:color "6c757d"]
              }
              ◊twitter["flyin1501" #:class "p-2"]{
                ◊font-awesome["twitter" #:color "6c757d"]
              }
              ◊github["kisaragi-hiu" #:class "p-2"]{
                ◊font-awesome["github" #:color "6c757d"]
              }
              ◊gitlab["kisaragi-hiu" #:class "p-2"]{
                ◊font-awesome["gitlab" #:color "6c757d"]
              }
              ◊youtube["channel/UCl_hsqcvdX0XdgBimRQ6R3A" #:class "p-2"]{
                ◊font-awesome["youtube-play" #:color "6c757d"]
              }
            </div>
          </div>
          <div class="col-4 nav-scroller py-1">
            <nav class="nav d-flex justify-content-end">
              <a class="p-2 text-secondary" href="/">Blog</a>
              <a class="p-2 text-secondary" href="/about.html">About</a>
              <a class="p-2 text-secondary" href="/all-tags.html">Tags</a>
              @(~> (map (lambda (tx) (attr-set* tx 'class "p-2 text-secondary"))
                        (tags->link/txexpr (get-language-tags all-tags)))
                   (map xexpr->html _)
                   (string-join _ "\n"))
              ◊; @(get-category-tags all-tags)
              ◊; <li><a href="@|uri-prefix|/categories.html">Categories</a></li>
            </nav>
          </div>
        </div>
      </header>

      ◊; Contents
      <div id="content" class="">
        @(if (index? contents)
             ◊; When the current page is an index
             (begin
               (let* ([indices (string->indices contents)]
                      [filtered-indices
                       (~>
                        (filter-not
                         (lambda (x) (equal? (content-ref x 'category) "Fiction"))
                         indices))]
                      [years (get-years-in-indices filtered-indices)]
                      ◊; tags are available in all-tags already
                      [categories (filter category? all-tags)])
                 ◊; Top section
                 (string-append
                   (cond [(special? tag)
                          (xexpr->string `(h1 ,(string-titlecase
                                                (tag-special-prefix tag))
                                              ": "
                                              (strong ,(strip-tag-special-prefix tag))))]
                         [tag
                          (xexpr->string `(h1 "Tag: " (strong ,tag)))]
                         [else
                          ◊; at index page
                          ◊; This is where the landing text should be
                          (xexpr->string
                           `(h1 ([class "blog-title"]) "Blog"))])
                   (string-join
                    ◊; for each year, grab the index items from that year
                    (map (lambda (year)
                           (string-append
                            (xexpr->string
                             `(div (a ([class "index-year text-secondary"]
                                       [data-toggle "collapse"]
                                       [href ,(~a "#collapseYear" year)]
                                       [role "button"]
                                       [aria-expanded "false"]
                                       [aria-controls ,(~a "collapseYear" year)])
                                      ,year)))
                            "<div class=\"collapse show\" id=\"collapseYear" year "\">"
                            (filter-indices-to-string
                             (lambda (x) (equal? (content-year x) year))
                             filtered-indices)
                            "</div>"))
                         years))
                   ◊; Indicies with the category "Fiction"
                   (let ([fiction-indices
                          (filter-indices-to-string
                           (lambda (x) (equal? (content-ref x 'category) "Fiction"))
                           indices)])
                     (if (equal? fiction-indices "")
                         ""
                         (string-append
                          (xexpr->string
                           `(h1 ([class "index-stream"]) "Fiction"))
                          fiction-indices))))))

             ◊; If not an index, just show the contents
             (strip-metadata contents))
      </div>

      ◊; Footer
      <footer class="">
        <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
           <img alt="Creative Commons License"
                style="border-width:0"
                src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" />
        </a>
        <br />
        <p>© Kisaragi Hiu 2017–2018. Posts are licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">CC-BY-SA 4.0 International license</a>.</p>
        ◊(->html ◊p{Site generated by ◊a[#:href "https://github.com/greghendershott/frog"]{Frog}, the ◊strong{fr}ozen bl◊strong{og} tool.})
        ◊(->html ◊p{Preprocessed with ◊a[#:href "http://pollenpub.com"]{Pollen}, the programmable publishing system.})
        ◊(->html ◊p{Using ◊a[#:href "https://getbootstrap.com"]{Bootstrap.}})
      </footer>
    </div>
    <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js"
            integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
            crossorigin="anonymous">
    </script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
            integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
            crossorigin="anonymous">
    </script>
    <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js"
            integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy"
            crossorigin="anonymous">
    </script>
    ◊; Google Analytics
    <script>
      window.ga = function () { ga.q.push(arguments) }; ga.q = []; ga.l = +new Date;
      ga('create', 'UA-XXXXX-Y', 'auto'); ga('send', 'pageview')
    </script>
    <script src="https://www.google-analytics.com/analytics.js" async defer></script>
  </body>
</html>
