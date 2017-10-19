<html>
  <head>
    <title>◊(select-from-metas 'headline here) - Kisaragi Hiu</title>

    ◊(->html (get-site-global-head #:livejs (select-from-metas 'livejs metas)
                                   #:justfont (if (select-from-metas 'justfont here)
                                                  (select-from-metas 'justfont metas)
                                                  #t)))

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
    ◊; When there is no publish date, 1) don't show page-meta, 2) make header push contents down
    ◊when/splice[(not (select-from-metas 'publish-date here))]{
    ◊(->html (get-site-header #:headline (select-from-metas 'headline here)
                              #:push-contents #t))
    }

    ◊(->html site-sidebar)

    ◊; When there is, show page-meta and header shouldn't push contents down
    ◊when/splice[(select-from-metas 'publish-date here)]{
        ◊(->html (get-site-header #:headline (select-from-metas 'headline here)))
        ◊(->html `(p ([class "page-meta"])
                     ,@(format-date (select-from-metas 'publish-date here))
                     " :: "
                     ,@(format-cats (select-from-metas 'categories here))))
    }

    ◊when/splice[(select-from-metas 'toc here)]{
        ◊(->html `(div ([id "toc"]) (h2 "Table of Contents")
                       ,@(select-from-doc 'toc-entries here)))
    }
    ◊(map ->html (select-from-doc 'body here))

    ◊(->html
      (let* ([newer (previous here)]
             [newer-title (select-from-metas 'headline newer)]
             [newer-href (string-append "/" (symbol->string newer))]
             [older (next here)]
             [older-title (select-from-metas 'headline older)]
             [older-href (string-append "/" (symbol->string older))])
         (get-site-prevnext #:newer newer
                            #:newer-title newer-title
                            #:newer-href newer-href
                            #:older older
                            #:older-title older-title
                            #:older-href older-href)))

    ◊when/splice[(select-from-metas 'comments here)]{
        <br>
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
    ◊(->html site/footer)
    </footer>
    ◊(->html site-global-end-of-body)
  </body>
</html>
