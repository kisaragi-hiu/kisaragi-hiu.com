<html>
  <head>
    <title>Kisaragi Hiu</title>

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

  </head>
  <body>
    ◊(->html (get-site-header #:at-index #t))

    ◊(->html site-sidebar)

    ◊(map (λ (x)
           (->html `(h2
                     (a ([href ,(symbol->string x)]
                         [class "readmore"])
                        ,(select-from-metas 'title x)))))
          (children here))

    <footer>
    ◊(->html site/footer)
    </footer>
    ◊(->html site-global-end-of-body)
  </body>
</html>
