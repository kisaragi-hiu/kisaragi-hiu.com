#lang pollen
◊; such a bloody hack
◊import["special-tags-for-index-and-post.template"]
@(local-require threading
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
<article>
  ◊article-header[#:date "@|date|" #:tags "@|filtered-tags|"
                  #:title "@|title|" #:uri "@|uri-path|"
                  #:category "@|category-from-tags|"
                  #:class "index-item"]
</article>
<hr>
