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
@(define date-without-year
    (~> (string->xexpr date)
        (map-elements (λ (x) (if (string? x)
                                 (substring x 5)
                                 x))
                      _)
        xexpr->string))
◊; See content-processing.rkt for metadata handling
{"type":"index",
 "date":"@(attr-ref (string->xexpr date) 'datetime)"}
<!-- end of metadata -->
<article class="index-item">
  ◊article-header[#:date "@|date-without-year|" #:tags "@|filtered-tags|"
                  #:title "@|title|" #:uri "@|uri-path|"
                  #:category "@|category-from-tags|"
                  #:class "index-header"]
</article>
<!-- end of index template -->

◊; Local Variables:
◊; mode: pollen
◊; End:
