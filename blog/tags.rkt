#lang racket

(require threading
         txexpr
         (only-in xml string->xexpr))

(provide (all-defined-out)
         (struct-out tag-st))

(struct tag-st (name url) #:transparent)

;; "tags":
;; '((tag-st "Tag1" "/tags/tag1.html")
;;   (tag-st "Tag2" "/tags/tag2.html"))
;; tag-string: `tags-list-items` format in page template
;; <li><a href="/tags/tag1.html">Tag1</a></li>
;; <li><a href="/tags/tag2.html">Tag2</a></li>
;; comma-html: `tags` format in index & post templates
;; <span><a href="/tags/tag1.html">Tag1</a>, <a href="/tags/tag2.html">Tag2</a></span>

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

(define (comma-html->tags str)
  (~> (string-replace str ", " "")
      string->xexpr
      get-elements
      (filter txexpr? _)
      (map (λ (x) (tag-st (last x) (attr-ref x 'href))) _)))

(define (tags->comma-html tags)
  (and~> (map (λ (x) (xexpr->html `(a ([href ,(tag-st-url x)]) ,(tag-st-name x))))
              tags)
         (add-between _ ", ")
         ((λ (lst) (if (empty? lst) #f lst)) _) ; short circuit out if it's empty
         (string-join _ "")
         (string-append "<span>" _ "</span>")))

(define (language? tag)
  (string-prefix? (tag-st-name tag) "language:"))

(define (category? tag)
  (string-prefix? (tag-st-name tag) "category:"))

(define (special? tag)
  (or (language? tag)
      (category? tag)))

(define (strip-tag-special-prefix tag)
  (tag-st (string-replace (tag-st-name tag) #rx"^.*:" "")
          (tag-st-url tag)))

(define (get-language-tags tags)
  (map (lambda (tag) (cond
                       [(string-prefix? (tag-st-name tag) "language:en")
                        (tag-st "English" (tag-st-url tag))]
                       [(string-prefix? (tag-st-name tag) "language:zh")
                        (tag-st "中文" (tag-st-url tag))]
                       [else (strip-tag-special-prefix tag)]))
       (filter language? tags)))

(define (get-category-tags tags)
  (map strip-tag-special-prefix
       (filter category? tags)))
