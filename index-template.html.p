<html>
  <head>
    <meta charset="UTF-8">
    <meta name="google" content="notranslate">
    <link rel="stylesheet" type="text/css" href="css/monokai.css" />
    <link rel="stylesheet" type="text/css" href="css/style.css" />
    <link rel="shortcut icon" href="favicon.ico" type="image/x-icon" />

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
    <div id="header">
      <a href="./index.html">
	<img src="images/avatar.png" />
      </a>
      <span class="righty"></span>
    </div>
◊(add-between (map (λ (x)
                    (->html `(div ([class "abstract"])
                                  (h2 ,(select 'h1 x))
                                  (p ([class "index-date"])
                                     "發佈於"
                                     ,@(format-date (select-from-metas 'publish-date x))
                                     " :: "
                                     ,@(format-cats (select-from-metas 'categories x)))
                                  (p ,@(get-elements (remove-supref (select-element 'p 'body x)))
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
