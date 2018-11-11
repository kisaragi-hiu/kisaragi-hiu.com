#lang pollen
@(local-require threading
                txexpr
                (only-in xml string->xexpr)
                "tags.rkt")
@(define filtered-tags
   (and~> (comma-html->tags tags)
          (filter-not special? _)
          tags->comma-html))
@(define category-from-tags
   (and~> (comma-html->tags tags)
          (filter category? _)
          ;; remove "category:" prefix
          (map (λ (x) (tag-st (string-replace (tag-st-name x) "category:" "") (tag-st-url x)))
            _)
          tags->comma-html))
@(define processed-date
   ◊; <time datetime="2018-07-27T00:00:00">07/27</time>
   (xexpr->string
    `(time
      ([datetime ,date-8601])
      ,(~> (substring date-8601 5)
           (string-replace _ #rx"T.*$" "")
           (string-replace _ "-" "/")))))
{"type":"post"}
<!-- end of metadata -->
<article>
  ◊article-header[#:date "@|processed-date|" #:tags "@|filtered-tags|"
                  #:title "@|title|" #:uri "@|full-uri|"
                  #:category "@|category-from-tags|"
                  #:class "post-header"]
  @|content|
  <footer>
    @twitter-share-button[full-uri]
    <a href="#top" id="top-link" >back to top</a>
    <div class="pager">
      <a href="@|older-uri|" id="pager-prev">@|older-title|</a>
      <a href="@|newer-uri|" id="pager-next">@|newer-title|</a>
    </div>
    @disqus-comments["kisaragi-hiu-blog"]
  </footer>
</article>

◊; Local Variables:
◊; mode: pollen
◊; End:
