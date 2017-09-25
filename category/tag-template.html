<html>
  <head>
    <title>Kisaragi Hiu/◊(select 'title here)</title>
    ◊(->html site-global-head)

    ◊when/block[(select-from-metas 'background here)]{
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
    ◊(->html (get-site-header))
    ◊(->html `(h1 "Category: " ,(select 'title here)))
    ◊(add-between (map (λ (x)
                         (->html `(div [[class "abstract"]]
                                       (h2 ,(select 'h1 x))
                                       (p ((class "index-date")) "Published on "
                                          ,@(format-date (select-from-metas 'publish-date x))
                                          " in "
                                          ,@(format-cats (select-from-metas 'categories x)))
                                       (p ,@(get-elements (select-element 'p 'body x))
                                          (br)
                                          (a [[href ,(string-append "../" (symbol->string x))]
                                              [class "readmore"]]
                                             "Read more")))))
                       (filter (λ (file)
                                 (tag-in-file? (select 'title here) file))
                               (children 'index.html)))
                  (->html `(hr)))
    
    <footer>
    </footer>
    
  </body>
</html>
