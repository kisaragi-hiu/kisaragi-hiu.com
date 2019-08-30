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
              families: ['Fira Sans', 'Overpass Mono', 'Overpass:400,600']
          }
      });
    </script>
    <link rel="stylesheet" href="/css/main.css">
    <link rel="alternate" type="application/atom+xml" href="/feed.xml" title="Atom Feed">
  </head>
  <body>
    <a id="top"></a>
    <div class="container">
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
          <a href="/projects.html">Projects</a>
          <a href="/about.html">About</a>
        </nav>
      </header>

      ◊; Contents
      <div id="content" class="">
        ◊(when/splice (string=? (document-type metas) "root-index")
          ◊list{
<p>I'm a college student interested in Free Software, programming, VOCALOID / UTAU culture, and language learning.</p>
})
        ◊(when/splice (string=? (document-type metas) "post")
           (to-html (index-item here)))
        ◊(when/splice (select-from-metas 'toc metas)
           (to-html (toc here)))
        ◊(to-html doc)
        ◊; (when/splice (string=? (document-type metas) "post")
        ◊;   (to-html (previous-and-next-same-category here)))
      </div>

      ◊; Footer
      <footer>
        <div id="footer-sep">・・・</div>
        ◊(when/splice (string=? (document-type metas) "post")
           (to-html (previous-and-next here)))
        <p>I don't necessarily know what I'm talking about.</p>
        ◊to-html{
        ◊ul{
          ◊li{◊link[◊(abs-local "feeds/all.atom.xml")]{Atom / RSS feed}}
          ◊li{◊twitter["flyin1501"]{Twitter}}
          ◊li{◊github["kisaragi-hiu"]{Github}}
          ◊li{◊gitlab["kisaragi-hiu"]{Gitlab.com}}
          ◊li{◊link["https://git.sr.ht/~kisaragi_hiu/"]{Sourcehut}}
          ◊li{◊youtube["channel/UCl_hsqcvdX0XdgBimRQ6R3A"]{Youtube}}
          ◊li{◊niconico["user/38995186"]{niconico}}
        }}
        <p>
          <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
           <img alt="Creative Commons License"
                style="border-width:0"
                src="/static/cc-by-sa-88x31.png" />
          </a>
        </p>
        ◊; @ tag acts as one tag but Pollen slices it into the document
        ◊to-html{◊@{
        ◊p{Posts are licensed under a ◊a[#:href "http://creativecommons.org/licenses/by-sa/4.0/"]{CC-BY-SA 4.0 International} license; ◊a[#:href "https://github.com/kisaragi-hiu/kisaragi-hiu.com"]{source code} is licensed under MIT. See ◊a[#:rel "license" #:href "https://github.com/kisaragi-hiu/kisaragi-hiu.com/blob/source/LICENSE.md"]{LICENSE.md} for details.}
        ◊p{© Kisaragi Hiu 2017~2019}
        }}
      </footer>
    </div>
  </body>
</html>
