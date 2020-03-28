<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <title>◊(extract-xexpr-strings (select-from-metas 'title metas)) | Kisaragi Hiu</title>
    <meta name="description" content="◊conf['desc]">
    <meta name="author" content="◊conf['author]">
    <meta name="keywords" content="◊conf['keywords]">
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
      // Prevent transitions from happening during load
      window.onload = () => {
        document.getElementsByTagName("body")[0].className = "";
      };
    </script>
    <!-- Fathom - simple website analytics - https://github.com/usefathom/fathom -->
    <script>
      let dnt = navigator.doNotTrack || navigator.msDoNotTrack || null;
      if (dnt !== "yes" && dnt !== "1") {
        (function (f, a, t, h, o, m) {
          a[h] =
            a[h] ||
            function () {
              (a[h].q = a[h].q || []).push(arguments);
            };
          (o = f.createElement("script")),
            (m = f.getElementsByTagName("script")[0]);
          o.async = 1;
          o.src = t;
          o.id = "fathom-script";
          m.parentNode.insertBefore(o, m);
        })(document, window, "//fathom.kisaragi-hiu.com/tracker.js", "fathom");
        fathom("set", "siteId", "FYSHR");
        fathom("trackPageview");
      }
    </script>
    <!-- / Fathom -->
    <link rel="stylesheet" href="/css/main.css">
    <link rel="alternate" type="application/atom+xml" href="/feed.xml" title="Atom Feed">
  </head>
  <body class="js-loading">
    <a id="top"></a>
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
      ◊; show heading for posts
      ◊(when/splice (string=? (post-type metas) "post")
         (to-html (post-heading here)))
      ◊; show toc if it exists
      ◊(when/splice (select-from-metas 'toc metas)
         (to-html (toc here)))
      ◊; actual content
      ◊(to-html doc)
      ◊(when/splice (string=? (post-type metas) "post")
         (to-html (previous-and-next here)))
      ◊(when/splice (string=? (post-type metas) "root-index")
         (to-html (index (children 'blog))))
    </div>

    ◊; Footer
    <footer id="sitefooter">
      <div id="footer-sep">・・・</div>
      <div id="sitefooter-content">
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
      ◊p{PGP fingerprint: ◊link[◊(abs-local "KisaragiHiu.asc")]{BCC7 4B10 41D4 B7D7 CC8B F402 40EC BEAE A877 5FC2}}
      ◊p{© Kisaragi Hiu 2017~2019.}
      ◊p{Posts are licensed under a ◊a[#:href "http://creativecommons.org/licenses/by-sa/4.0/"]{CC-BY-SA 4.0 International} license.}
      ◊p{◊a[#:href "https://github.com/kisaragi-hiu/kisaragi-hiu.com"]{Source code} is licensed under MIT. See ◊a[#:rel "license" #:href "https://github.com/kisaragi-hiu/kisaragi-hiu.com/blob/source/LICENSE.md"]{LICENSE.md} for details.}
      }}
      </div>
    </footer>
  </body>
</html>
