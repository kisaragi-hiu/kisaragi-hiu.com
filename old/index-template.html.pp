#lang pollen
@(local-require threading
                txexpr
                (only-in xml string->xexpr)
                "tags.rkt")
@; and~> because tags->comma-html will be #f if there's no tags/categories
@(define filtered-tags
   (and~> (comma-html->tags tags)
          (filter-not special? _)
          (tags->comma-html _)))
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
◊; See content-processing.rkt for metadata handling
{"type":"index",
 "date":"@(attr-ref (string->xexpr date) 'datetime)",
 "tags":"@(comma-html->tags tags)",
 "filtered-tags":"@(comma-html->tags filtered-tags)",
 "category":"@(and~> category-from-tags comma-html->tags first tag-st-name)"}
<!-- end of metadata -->
<article class="index-item">
  ◊article-header[#:date "@|processed-date|" #:tags "@|filtered-tags|"
                  #:title "@|title|" #:uri "@|uri-path|"
                  #:category "@|category-from-tags|"
                  #:class "index-header"]
</article>
<!-- end of index template -->

◊; Local Variables:
◊; mode: pollen
◊; End:
