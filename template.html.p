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
    <link rel="stylesheet"
          href="https://fonts.googleapis.com/css?family=Fira+Sans%7COverpass+Mono%7COverpass:400,600">
    <link rel="stylesheet"
          href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css"
          integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO"
          crossorigin="anonymous">
    <link rel="stylesheet" href="/css/emacs.css">
    <link rel="stylesheet" href="/css/main.css">
    <link rel="alternate" type="application/atom+xml" href="/feed.xml" title="Atom Feed">
  </head>
  <body>
    <!--[if lte IE 9]>
      ◊; … too aggressive?
      <h1>For the love of god, please stop using IE9. Thanks.</h1>
    <![endif]-->
    <a id="top"></a>
    <div class="container">
      ◊; Header
      <header id="header" class="py-2">
        <div class="row flex-nowrap justify-content-between alien-items-center">
          <div id="logo" class="col-6 pt-1">
            <div class="d-flex justify-content-begin">
              <a href="/" target="_self" class="py-2 pr-2"><img src="/images/avatar.png" alt="Kisaragi Hiu"/></a>
              <h1 style="margin-top: 0.4em;">Kisaragi&nbsp;Hiu</h1>
            </div>
          </div>
          <div class="col-6 nav-scroller py-1">
            <nav class="nav d-flex justify-content-end">
              <a class="p-2 text-secondary" href="/">Blog</a>
              <a class="p-2 text-secondary" href="/projects.html">Projects</a>
              <a class="p-2 text-secondary" href="/about.html">About</a>
            </nav>
          </div>
        </div>
      </header>

      ◊; Contents
      <div id="content" class="">
        ◊(when/splice (string=? (document-type metas) "root-index")
          ◊list{

◊to-html{
◊div[#:class "d-flex justify-content-begin"]{
  ◊link["@|atom-feed-uri|" #:class "py-2"]{
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
        ◊(to-html doc)
        ◊(when/splice (string=? (document-type metas) "post")
           (to-html (div '([class "prev-and-next-div"])
                          (previous-and-next here)
                          (previous-and-next-same-category here))))
      </div>

      ◊; Footer
      <footer class="">
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
    ◊;<script>
      ◊;  window.ga = function () { ga.q.push(arguments) }; ga.q = []; ga.l = +new Date;
      ◊;  ga('create', 'UA-109874076-1', 'auto'); ga('send', 'pageview')
    ◊;</script>
    ◊;<script src="https://www.google-analytics.com/analytics.js" async defer></script>
  </body>
</html>
