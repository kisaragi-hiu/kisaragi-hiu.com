<html>
  <head>
    <title>◊(select-from-metas 'headline here)</title>
    ◊(->html site-global-head)

    ◊when/splice[(select-from-metas 'background here)]{
    <style>
      #header {
      background-image: url(◊(select-from-metas 'background metas));
      background-size: cover;
      background-position: center top;
      margin-bottom: 6em;
      }
    </style>
    }

    ◊when/splice[(select-from-metas 'language here)]{
    <link rel="stylesheet" href="◊(get-language-stylesheet (select-from-metas 'language metas))"/>
    }

  </head>
  <body>
    ◊(->html (get-site-header #:headline (select-from-metas 'headline here)))
    ◊when/splice[(select-from-metas 'date here)]{
        ◊(->html `(p ([class "date"])
                     ,@(format-date (select-from-metas 'publish-date here))))
    }

    ◊when/splice[(select-from-metas 'categories here)]{
        ◊(->html `(div ([class "category"])
                       ,@(format-cats (select-from-metas 'categories here))))
    }

    ◊when/splice[(select-from-metas 'toc here)]{
        ◊(->html `(div ([id "toc"]) (h2 "Table of Contents")
                       ,@(select-from-doc 'toc-entries here)))
    }
    ◊(map ->html (select-from-doc 'body here))

    ◊when/splice[(select-from-metas 'comments here)]{
        ◊(->html pagebreak)
        <h2>Comments</h2>
        <div id="disqus_thread"></div>
        <script>
            var disqus_config = function () {
                this.page.identifier = '◊(string-append (select-from-metas 'headline here) (select-from-metas 'publish-date here))';
            };

            (function() {  // REQUIRED CONFIGURATION VARIABLE: EDIT THE SHORTNAME BELOW
                var d = document, s = d.createElement('script');

                s.src = 'https://kisaragi-hiu-blog.disqus.com/embed.js';

                s.setAttribute('data-timestamp', +new Date());
                (d.head || d.body).appendChild(s);
            })();
        </script>
        <noscript>
            Enable JavaScript if you want to see
            <a href="https://disqus.com/?ref_noscript" rel="nofollow">
                Disqus comments.
            </a>
        </noscript>
    }
    <footer>
    </footer>

  </body>
</html>
