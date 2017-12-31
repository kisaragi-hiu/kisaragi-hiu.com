#lang pollen
◊; such a bloody hack
◊import["special-tags-for-index-and-post.template"]
<article>
  ◊article-header[#:date "@|date|" #:tags "@|filtered-tags|"
                  #:title "@|title|" #:uri "@|full-uri|"
                  #:class "post-header"]
  @|content|
  <footer>
    @twitter-share-button[full-uri]
    @google-plus-share-button[full-uri]
    @older/newer-links[older-uri older-title newer-uri newer-title]
    ◊google-adsense/responsive
    <br>
    @disqus-comments["kisaragi-hiu-blog"]
  </footer>
</article>
