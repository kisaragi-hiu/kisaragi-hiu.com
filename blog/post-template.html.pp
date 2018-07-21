#lang pollen
@(local-require threading
                txexpr
                (only-in xml string->xexpr)
                "tags.rkt")
@(define filtered-tags
   (~> (comma-html->tags tags)
       (filter-not special? _)
       tags->comma-html))
@(define category-from-tags
   (~> (comma-html->tags tags)
       (filter category? _)
       ;; remove "category:" prefix
       (map (λ (x) (tag-st (string-replace (tag-st-name x) "category:" "") (tag-st-url x)))
            _)
       tags->comma-html))
@(define processed-date
    (~> (string->xexpr date)
        (map-elements (λ (x) (if (string? x) (string-replace x "-" "/") x)) _)
        xexpr->string))
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
    @google-plus-share-button[full-uri]
    @older/newer-links[older-uri older-title newer-uri newer-title]
    ◊google-adsense/responsive
    <br>
    @disqus-comments["kisaragi-hiu-blog"]
  </footer>
</article>

◊; Local Variables:
◊; mode: pollen
◊; End:
