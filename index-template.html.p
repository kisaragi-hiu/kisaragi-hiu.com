<html>
  <head>
    <title>Kisaragi Hiu</title>
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

  </head>
  <body>
    ◊(->html (get-site-header #:at-index #t))
    ◊(add-between (map (λ (x)
                        (->html `(div ([class "abstract"])
                                      (h2 ,(select-from-metas 'headline x))
                                      (p ([class "page-meta"])
                                         "發佈於"
                                         ,@(format-date (select-from-metas 'publish-date x))
                                         " :: "
                                         ,@(format-cats (select-from-metas 'categories x)))
                                      (p ,@(get-elements (remove-supref
                                                          (select-element 'p 'body x)))
                                         (br)
                                         (a ([href ,(symbol->string x)]
                                             [class "readmore"])
                                             "更多")))))
                       (children here))
                  (->html `(hr)))

    <footer>
    </footer>

  </body>
</html>
