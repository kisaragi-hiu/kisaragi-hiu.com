<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    <title>◊(extract-xexpr-strings (select-from-metas 'title metas)) | Kisaragi Hiu</title>
    <meta name="description" content="◊|description|">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="author" content="◊|author|">
    <meta name="keywords" content="Coding, Programming, Language, LGBT, Blog">
    <link rel="icon" href="◊abs-local{favicon.ico}">
    <link rel="canonical" href="◊(abs-global here)">
    ◊; Font
    <script src="https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"></script>
    <script>
      WebFont.load({
          google: {
              families: ['Overpass Mono', 'Overpass:200,400,600']
          }
      });
    </script>
    <link rel="stylesheet" href="/css/main.css">
    <link rel="alternate" type="application/atom+xml" href="/feed.xml" title="Atom Feed">
  </head>
  <body>
    <a id="top"></a>
    <!--[if lte IE 9]>
      ◊; … too aggressive?
      <h1>For the love of god, please stop using IE9. Thanks.</h1>
    <![endif]-->
    ◊; Header
    <header id="siteheader">
      <div id="logo">
        <a href="/">
          <img src="/static/avatar.png" alt="Kisaragi Hiu"/>
        </a>
        <h1>Kisaragi&nbsp;Hiu</h1>
      </div>
      <nav>
        <a href="/">Blog</a>
        <a href="/category.html">Categories</a>
        <a href="/projects.html">Projects</a>
        <a href="/about.html">About</a>
      </nav>
    </header>

    ◊; Contents
    <div id="content">
      ◊(when/splice (string=? (document-type metas) "root-index")
        ◊list{
<p>I'm a college student interested in Free Software, programming, VOCALOID / UTAU culture, and language learning.</p>
})
      ◊(when/splice (string=? (document-type metas) "post")
         (to-html (index-item here)))
      ◊(when/splice (and (select-from-metas 'title metas)
                         (not (string=? (document-type metas) "category-index")))
         (to-html `(h1 ,(select-from-metas 'title metas))))
      ◊(when/splice (select-from-metas 'toc metas)
         (to-html (toc here)))
      ◊(to-html doc)
      ◊; (when/splice (string=? (document-type metas) "post")
      ◊;   (to-html (previous-and-next-same-category here)))
    </div>

    ◊; Footer
    <footer id="sitefooter">
      <div id="footer-sep">・・・</div>
      <div id="sitefooter-content">
      ◊(when/splice (string=? (document-type metas) "post")
         (to-html (previous-and-next here)))
      <p>I don't necessarily know what I'm talking about.</p>
      <nav>
        ◊to-html{◊@{
        ◊link[◊(abs-local "feeds.html")]{◊icon{rss}}
        ◊twitter["flyin1501"]{◊icon{twitter}}
        ◊github["kisaragi-hiu"]{◊icon{github}}
        ◊gitlab["kisaragi-hiu"]{◊icon{gitlab}}
        ◊youtube["channel/UCl_hsqcvdX0XdgBimRQ6R3A"]{◊icon{youtube}}
        ◊link["/about.html#links"]{◊icon{more-horizontal}}
        }}
      </nav>
      ◊to-html{◊@{
      ◊p{PGP fingerprint: ◊link[◊(abs-local "KisaragiHiu.gpg")]{BCC7 4B10 41D4 B7D7 CC8B F402 40EC BEAE A877 5FC2}}
      ◊p{© Kisaragi Hiu 2017~2019.}
      ◊p{Posts are licensed under a ◊a[#:href "http://creativecommons.org/licenses/by-sa/4.0/"]{CC-BY-SA 4.0 International} license.}
      ◊p{◊a[#:href "https://github.com/kisaragi-hiu/kisaragi-hiu.com"]{Source code} is licensed under MIT. See ◊a[#:rel "license" #:href "https://github.com/kisaragi-hiu/kisaragi-hiu.com/blob/source/LICENSE.md"]{LICENSE.md} for details.}
      }}
      </div>
    </footer>
  </body>
</html>
