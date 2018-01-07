#lang pollen
◊; such a bloody hack
◊import["special-tags-for-index-and-post.template"]
<article>
  ◊article-header[#:date "@|date|" #:tags "@|filtered-tags|"
                  #:title "@|title|" #:uri "@|uri-path|"
                  #:category "@|category-from-tags|"
                  #:class "index-item"]
</article>
<br>
