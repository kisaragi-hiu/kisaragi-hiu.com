#lang pollen
◊; This file is fed through pollen first.
◊; This `require`s eg. ->html for use as a preprocessor.
◊(require pollen/template)

@;{ This is a frog template comment. }

@;{ This needs local-require as it's a template }
@(local-require (only-in xml string->xexpr)
                txexpr threading racket/format racket/string)

@;{ `tag` is already taken }
@(struct tag-st (name url) #:transparent)

@;{ tags-list-items looks like
<li><a href="/tags/tag1.html">Tag1</a></li>
<li><a href="/tags/tag2.html">Tag2</a></li>

This transforms it into all-tags in the form
'((tag-st "Tag1" "/tags/tag1.html")
  (tag-st "Tag2" "/tags/tag2.html"))
}

@(define all-tags
   (~> (string-append "<tags>" tags-list-items "</tags>") ; force a top level tag needed by string->xexpr
       string->xexpr
       get-elements ; strip away the top level tag
       (filter txexpr? _) ; strip away the leftover newlines between each element
       (map (λ (x) (first (get-elements x))) _) ; extract the a tag
       (map (λ (x) (tag-st (last x) (attr-ref x 'href))) _)
   ))

@(define (get-language-tags tags)
   (map (lambda (tag) (cond
                        [(string-prefix? (tag-st-name tag) "language:en") (tag-st "English" (tag-st-url tag))]
                        [(string-prefix? (tag-st-name tag) "language:zh") (tag-st "中文" (tag-st-url tag))]
                        [else tag]))
        (filter (λ (x) (string-prefix? (tag-st-name x) "language:")) tags)))

@(define (taglist->li-a taglist)
   ; listof tag-st -> string
   (~> (map (λ (x) (xexpr->html `(li (a ([href ,(tag-st-url x)]) ,(tag-st-name x)))))
            taglist)
       (string-join _ "\n")))

<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>@|title|</title>
    <meta name="description" content="@|description|">
    <meta name="author"      content="@|author|">
    <meta name="keywords"    content="@|keywords|">
    <meta name="viewport"    content="width=device-width, initial-scale=1">
    <link rel="icon"      href="@|uri-prefix|/favicon.ico">
    <link rel="canonical" href="@|full-uri|">

    @(when rel-next @list{<link rel="next" href="@|rel-next|">})
    @(when rel-prev @list{<link rel="prev" href="@|rel-prev|">})
    <!-- Font -->
    <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Fira+Sans|Overpass">

    <!-- CSS -->
    <link rel="stylesheet" type="text/css" href="@|uri-prefix|/css/normalize.css">
    <link rel="stylesheet" type="text/css" href="@|uri-prefix|/css/skeleton.css">
    <link rel="stylesheet" type="text/css" href="@|uri-prefix|/css/monokai.css">
    <link rel="stylesheet" type="text/css" href="@|uri-prefix|/css/custom.css">
    <!-- Feeds -->
    <link rel="alternate" type="application/atom+xml"
          href="@|atom-feed-uri|" title="Atom Feed">
    <!-- JS -->
    <script src="https://use.fontawesome.com/f9f3cd1f14.js"></script>
    @google-universal-analytics["UA-109874076-1"]
    ◊;google-adsense/page-level["ca-pub-6215394828182929"]
  </head>
  <body>
    <div class="container">
      <header id="topheader" class="ten columns offset-by-one">
        <div class="logo">
          <a href="/"><img src="/images/text-logo.svg" alt="如月.飛羽"></img></a>
          <img src="@|uri-prefix|/images/avatar.png"/>
        </div>
        <nav>
          <ul>
            <li><a href="@|uri-prefix|/index.html">Blog</a></li>
            <li><a href="@|uri-prefix|/about.html">About</a></li>
            @(taglist->li-a (get-language-tags all-tags))
          </ul>
        </nav>
      </header>

      <div class="row">
        <div id="content" class="ten columns offset-by-one">
          @;{ tags with ":" are special tags, ignore them for now }
          @(when (and tag
                      (not (string-contains? tag ":")))
            @list{<h1>Tag: <em>@|tag|</em></h1>})

          @|contents|
        </div>
      </div>

      <footer class="ten columns offset-by-one">
        <hr />
        <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
           <img alt="Creative Commons License"
                style="border-width:0"
                src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" />
        </a>
        <br />
        <p>© Kisaragi Hiu 2017. Posts are licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">CC-BY-SA 4.0 International license</a>.</p>
        <ul id="social-links">
          ◊; ◊font-awesome returns a string so it can be used in a document
          ◊; here we want raw txexpr for input into ◊L and ◊link
          <li>◊link["@|atom-feed-uri|"]{◊font-awesome["fa-rss-square" #:return-txexpr? #t]}</li>
          <li>◊twitter["flyin1501"]{◊font-awesome["fa-twitter-square" #:return-txexpr? #t]}</li>
          <li>◊github["flyingfeather1501"]{◊font-awesome["fa-github-square" #:return-txexpr? #t]}</li>
          <li>◊youtube["channel/UCl_hsqcvdX0XdgBimRQ6R3A"]{◊font-awesome["fa-youtube-square" #:return-txexpr? #t]}</li>
        </ul>
        ◊(->html ◊p{Site generated by ◊a[#:href "https://github.com/greghendershott/frog"]{Frog}, the ◊strong{fr}ozen bl◊strong{og} tool.})
        ◊(->html ◊p{Preprocessed with ◊a[#:href "http://pollenpub.com"]{Pollen}, the programmable publishing system.})
        ◊(->html ◊p{Style based on ◊a[#:href "http://getskeleton.com/"]{Skeleton}.})
      </footer>
    </div>
  </body>
</html>
