#lang racket

(require threading
         txexpr
         "widgets.rkt"
         (only-in xml
                  string->xexpr))

(provide (all-defined-out))
         ;; (struct-out tag-st))

;; (struct tag-st (name url) #:transparent)

;; So that strings are tag-st as well…
;; Probably doable with structs, but I don't understand them enough
(define (tag-st name url) (cons name url))
(define (tag-st? tag) (or (cons? tag) (string? tag)))
(define (tag-st-name tag)
  (cond [(string? tag) tag]
        [(cons? tag) (car tag)]
        [else #f]))
(define (tag-st-url tag)
  (cond [(cons? tag) (cdr tag)]
        [else #f]))

;; "tags":
;; '((tag-st "Tag1" "/tags/tag1.html")
;;   (tag-st "Tag2" "/tags/tag2.html"))
;; tag-string: `tags-list-items` format in page template
;; <li><a href="/tags/tag1.html">Tag1</a></li>
;; <li><a href="/tags/tag2.html">Tag2</a></li>
;; comma-html: `tags` format in index & post templates
;; <span><a href="/tags/tag1.html">Tag1</a>, <a href="/tags/tag2.html">Tag2</a></span>

(define (tags->jsexpr tags)
  (~> (map (λ (x) (cons (string->symbol (tag-st-name x))
                        (tag-st-url x)))
           tags)
      make-hash))

(define (jsexpr->tags jsexpr)
  (hash-map jsexpr (λ (k v) (tag-st (symbol->string k) v))))

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

(define (tags->link tags)
  (~> (map (λ (x) (xexpr->html `(a ([href ,(tag-st-url x)]) ,(tag-st-name x))))
           tags)
      (string-join _ "\n")))

;; tags->link but as a txexpr
(define (tags->link/txexpr tags)
  (map (λ (x) `(a ([href ,(tag-st-url x)]) ,(tag-st-name x)))
       tags))

(define (seperator-html->tags str [seperator ", "])
  (and~> str
         (string-replace _ seperator "")
         string->xexpr
         get-elements
         (filter txexpr? _)
         (map (λ (x) (tag-st (last x) (attr-ref x 'href))) _)))

(define (tags->seperator-html tags [seperator ", "])
  (and~> tags
         (map (λ (x) (xexpr->html `(a ([href ,(tag-st-url x)]) ,(tag-st-name x)))) _)
         (add-between _ seperator)
         ((λ (lst) (if (empty? lst) #f lst)) _) ; short circuit out if it's empty
         (string-join _ "")
         (string-append "<span>" _ "</span>")))

(define (tags->comma-html tags)
  (tags->seperator-html tags ", "))

(define (comma-html->tags str)
  (seperator-html->tags str ", "))

(define (language? tag)
  (string-prefix? (tag-st-name tag) "language:"))

(define (category? tag)
  (string-prefix? (tag-st-name tag) "category:"))

(define (special? tag)
  (and (tag-st? tag)
       (not (not ; cast to boolean
             (regexp-match #rx"^.*:" (tag-st-name tag))))))

;; tag-st? -> string?
(define (tag-special-prefix tag)
  (string-replace (tag-st-name tag) #rx":.*$" ""))

;; TODO: rename this… thing. I've been confused by it a few times now.
;; tag-st? -> tag-st?
;; (define (tag-strip-special-prefix tag)
(define (strip-tag-special-prefix tag)
  (cond [(string? tag) (string-replace tag #rx"^.*:" "")]
        [else (tag-st (string-replace (tag-st-name tag) #rx"^.*:" "")
                      (tag-st-url tag))]))

;; (filter language? tags) then convert their names for display
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

;;; Unit Tests
(module+ test
  (require rackunit)
  (check-equal? (tag-string->tags
                 "<li><a href=\"/tags/a.html\">a</a></li>
<li><a href=\"/tags/b.html\">b</a></li>")
                (list (tag-st "a" "/tags/a.html")
                      (tag-st "b" "/tags/b.html")))

  (check-equal? (tags->tag-string (list (tag-st "a" "/tags/a.html")
                                        (tag-st "b" "/tags/b.html")))
                "<li><a href=\"/tags/a.html\">a</a></li>
<li><a href=\"/tags/b.html\">b</a></li>")

  (check-equal? (tags->link (list (tag-st "a" "/tags/a.html")
                                  (tag-st "b" "/tags/b.html")))
                "<a href=\"/tags/a.html\">a</a>
<a href=\"/tags/b.html\">b</a>")

  (check-equal? (tags->comma-html (list (tag-st "a" "/tags/a.html")
                                        (tag-st "b" "/tags/b.html")))
                "<span><a href=\"/tags/a.html\">a</a>, <a href=\"/tags/b.html\">b</a></span>")

  (check-equal? (comma-html->tags "<span><a href=\"/tags/a.html\">a</a>, <a href=\"/tags/b.html\">b</a></span>")
                (list (tag-st "a" "/tags/a.html")
                      (tag-st "b" "/tags/b.html")))

  (check-true (language? (tag-st "language:en" "en.html")))
  (check-false (language? (tag-st "category:en" "en.html")))
  (check-false (language? (tag-st "en" "en.html")))

  (check-true (category? (tag-st "category:en" "en.html")))
  (check-false (category? (tag-st "language:en" "en.html")))
  (check-false (category? (tag-st "en" "en.html")))

  (check-true (special? (tag-st "special:abc" "abc.html")))
  (check-true (special? (tag-st "category:abc" "abc.html")))
  (check-true (special? (tag-st "language:abc" "abc.html")))
  (check-false (special? (tag-st "abc" "abc.html")))

  (check-equal? (strip-tag-special-prefix (tag-st "special:a" "a.html"))
                (tag-st "a" "a.html"))

  (check-equal? (get-language-tags (list (tag-st "language:en" "en.html")
                                         (tag-st "language:zh-tw" "zh-tw.html")
                                         (tag-st "language:xyz" "xyz.html")
                                         (tag-st "category:abc" "abc.html")
                                         (tag-st "c" "c.html")))
                (list (tag-st "English" "en.html")
                      (tag-st "中文" "zh-tw.html")
                      (tag-st "xyz" "xyz.html")))

  (check-equal? (get-category-tags (list (tag-st "category:a" "a.html")
                                         (tag-st "category:b" "b.html")
                                         (tag-st "c" "c.html")))
                (list (tag-st "a" "a.html")
                      (tag-st "b" "b.html"))))
