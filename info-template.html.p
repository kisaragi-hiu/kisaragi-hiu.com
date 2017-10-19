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

        <br>
        ◊(->html pagebreak)
    <footer>
    ◊(->html site/footer)
    </footer>
    ◊(->html site-global-end-of-body)
  </body>
</html>
