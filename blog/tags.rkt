#lang racket

(require threading
         txexpr
         (only-in xml string->xexpr))

(provide tag-string->tags
         tags->tag-string
         get-language-tags
         (struct-out tag-st))

(struct tag-st (name url) #:transparent)

;; tag-string:
;; <li><a href="/tags/tag1.html">Tag1</a></li>
;; <li><a href="/tags/tag2.html">Tag2</a></li>
;; "tags":
;; '((tag-st "Tag1" "/tags/tag1.html")
;;   (tag-st "Tag2" "/tags/tag2.html"))

(define (tag-string->tags str)
  (~> (string-append "<tags>" str "</tags>") ; force a top level tag needed by string->xexpr
      string->xexpr
      get-elements ; strip away the top level tag
      (filter txexpr? _) ; strip away the leftover newlines between each element
      (map (λ (x) (first (get-elements x))) _) ; extract the a tag
      (map (λ (x) (tag-st (last x) (attr-ref x 'href))) _)))

(define (tags->tag-string tags)
  (~> (map (λ (x) (xexpr->html `(li (a ([href ,(tag-st-url x)]) ,(tag-st-name x)))))
           tags)
      (string-join _ "\n")))

(define (get-language-tags tags)
  (map (lambda (tag) (cond
                       [(string-prefix? (tag-st-name tag) "language:en")
                        (tag-st "English" (tag-st-url tag))]
                       [(string-prefix? (tag-st-name tag) "language:zh")
                        (tag-st "中文" (tag-st-url tag))]
                       [else tag]))
       (filter (λ (x) (string-prefix? (tag-st-name x) "language:")) tags)))
