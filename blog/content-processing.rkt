#lang racket

(require threading
         json)

(provide (all-defined-out))

;; content metadata comes from the definitions in index and post templates.
;; The only information about them accessable page template is the final
;; @contents string, so to make it easier to parse, I'm putting some useful
;; information in a metadata section in there, as a JSON string.

(define metadata-marker "<!-- end of metadata -->")
(define index-template-marker "<!-- end of index template -->")

(define (content-metadata content)
  ;; Extract the metadata from content as a jsexpr.
  (if (string-contains? content metadata-marker)
      (string->jsexpr
       (first (string-split content metadata-marker)))
      (hasheq)))

(define (strip-metadata content)
  ;; Strip away the metadata from the content.
  (cond [(string-contains? content metadata-marker)
         (second (string-split content metadata-marker))]
        [else content]))

(define (index? content)
  ;; Does this content declare it's from the index template?
  (equal? (dict-ref (content-metadata content) 'type #f) "index"))

(define (post? content)
  ;; Does this content declare it's from the post template?
  (equal? (dict-ref (content-metadata content) 'type #f) "post"))


(define (content-date content)
  (content-ref content 'date))

(define (content-year content)
  (date-string-year (content-date content)))

(define (date-string-year str)
  ;; "2018-06-17" -> "2018"
  ;; assumes 4 digit year :)
  (substring str 0 4))

(define (date-string-month-day str)
  ;; assumes 4 digit year :)
  (substring str 5))

(define (string->indices str)
  ;; split content from index-template into individual indices
  (string-split (string-trim str) index-template-marker))

(define (get-years-in-indices lst)
  ;; get the years that are in the indices
  (remove-duplicates (map content-year lst)))

