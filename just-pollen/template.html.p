<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="x-ua-compatible" content="ie=edge">
    <title>◊(select-from-metas 'title metas)</title>
    <meta name="description" content="◊|description|">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
    <meta name="author" content="◊|author|">
    <meta name="keywords" content="◊|keywords|">
    <link rel="icon" href="/favicon.ico">
    <link rel="canonical" href="◊|full-uri|">

    ◊;(when rel-next @list{<link rel="next" href="@|rel-next|">})
    ◊;(when rel-prev @list{<link rel="prev" href="@|rel-prev|">})
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
      </header>

      ◊; Contents
      <div id="content" class="">
        ◊(->html doc)
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
