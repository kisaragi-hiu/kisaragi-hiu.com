<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    <title>◊(string-titlecase (extract-xexpr-strings (select-from-metas 'title metas))) | Kisaragi Hiu</title>
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
    <link rel="stylesheet" href="/css/emacs.css">
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

◊to-html{
◊div[#:class "d-flex justify-content-begin"]{
  ◊link[◊(abs-local "feeds/all.atom.xml") #:class "py-2"]{
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
}}

<p>I'm a college student interested in Free Software, programming, VOCALOID / UTAU culture, and language learning.</p>

<p>Contact:<br>
contact@kisaragi-hiu.com</p>
})
        ◊(when/splice (string=? (document-type metas) "post")
           (to-html (index-item here)))
        ◊(when/splice (select-from-metas 'toc metas)
           (to-html (toc here)))
        ◊(to-html doc)
        ◊(when/splice (string=? (document-type metas) "post")
           (to-html (previous-and-next here)))
        ◊; (when/splice (string=? (document-type metas) "post")
        ◊;   (to-html (previous-and-next-same-category here)))
      </div>

      ◊; Footer
      <footer class="">
      </footer>
    </div>
    ◊; Google Analytics
    <script>
      window.ga = function () { ga.q.push(arguments) }; ga.q = []; ga.l = +new Date;
      ga('create', 'UA-109874076-1', 'auto'); ga('send', 'pageview')
    </script>
    <script src="https://www.google-analytics.com/analytics.js" async defer></script>
  </body>
</html>
