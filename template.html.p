<html>
  <head>
    <title>◊(remove-tags (select 'headline here))</title>
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
    ◊(->html site-header)
    ◊(->html `(p ([class "date"]) ,@(format-date (select-from-metas 'publish-date here))))
    ◊(->html (select 'headline here))

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
        <h2>Comments</h2>
        ◊;disqus comments here
    }
    <footer>
    </footer>
    
  </body>
</html>
