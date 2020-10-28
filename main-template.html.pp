#lang pollen
◊(require "rkt/template.rkt")
◊; possible pollen bug: template.html.pp is used as if it is template.html, causing errors
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <title>◊"◊"(extract-xexpr-strings (select-from-metas 'title metas)) | Kisaragi Hiu</title>
    <meta name="description" content="◊conf['desc]">
    <meta name="author" content="◊conf['author]">
    <meta name="keywords" content="◊conf['keywords]">
    <link rel="icon" href="◊abs-local{favicon.ico}">
    <link rel="canonical" href="◊"◊"(abs-global here)">
    <link rel="stylesheet" href="/css/main.css">
    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&family=Overpass+Mono&display=swap" rel="stylesheet">
    ◊to-html[(script-analytics "FYSHR")]
    <link rel="alternate" type="application/atom+xml" href="/feed.xml" title="Atom Feed">
  </head>
  <body>
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
    <div id="content">
      ◊; show heading for posts
      ◊"◊"(when/splice (string=? (post-type metas) "post")
            (to-html (post-heading here)))
      ◊; show toc if it exists
      ◊"◊"(when/splice (select-from-metas 'toc metas)
            (to-html (toc here)))
      ◊; actual content
      ◊"◊"(to-html doc)
    </div>

    ◊; Footer
    <footer id="sitefooter">
      <div id="footer-sep">・・・</div>
      ◊"◊"(when/splice (string=? (post-type metas) "post")
            (to-html (previous-and-next here)))
      <div id="sitefooter-content">
      <p>I don't necessarily know what I'm talking about.</p>
      <nav>
        ◊to-html{◊link[◊(abs-local "feeds.html")]{◊icon{rss}}}
        ◊to-html{◊twitter["flyin1501"]{◊icon{twitter}}}
        ◊to-html{◊github["kisaragi-hiu"]{◊icon{github}}}
        ◊to-html{◊gitlab["kisaragi-hiu"]{◊icon{gitlab}}}
        ◊to-html{◊youtube["channel/UCl_hsqcvdX0XdgBimRQ6R3A"]{◊icon{youtube}}}
        ◊to-html{◊link["/about.html#links"]{◊icon{more-horizontal}}}
      </nav>
      <p>© Kisaragi Hiu 2017~2019.</p>
      <p>Posts are licensed under a <a href="http://creativecommons.org/licenses/by-sa/4.0/">CC-BY-SA 4.0 International</a> license.</p>
      <p><a href="https://github.com/kisaragi-hiu/kisaragi-hiu.com">Source code</a> is licensed under MIT. See <a rel="license" href="https://github.com/kisaragi-hiu/kisaragi-hiu.com/blob/source/LICENSE.md">LICENSE.md</a> for details.</p>
      </div>
    </footer>
  </body>
</html>
