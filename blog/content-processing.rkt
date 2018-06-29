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

(define (content-metadata contents)
  ;; Extract the metadata from contents as a jsexpr.
  (if (string-contains? contents metadata-marker)
      (string->jsexpr
       (first (string-split contents metadata-marker)))
      (hasheq)))

(define (strip-metadata contents)
  ;; Strip away the metadata from the contents.
  (cond [(string-contains? contents metadata-marker)
         (second (string-split contents metadata-marker))]
        [else contents]))

(define (index? contents)
  ;; Does this content declare it's from the index template?
  (equal? (dict-ref (content-metadata contents) 'type #f) "index"))

(define (post? contents)
  ;; Does this content declare it's from the post template?
  (equal? (dict-ref (content-metadata contents) 'type #f) "post"))

(define (content-date contents)
  (dict-ref (content-metadata contents) 'date #f))

(define (content-year contents)
  (date-string-year (content-date contents)))

(define (date-string-year str)
  ;; "2018-06-17" -> "2018"
  ;; assumes 4 digit year :)
  (substring str 0 4))

(define (date-string-month-day str)
  ;; assumes 4 digit year :)
  (substring str 5))

(define (string->indices str)
  ;; split contents from index-template into individual indices
  (string-split (string-trim str) index-template-marker))

(define (get-years-in-indices lst)
  ;; get the years that are in the indices
  (remove-duplicates (map (compose1 date-string-year content-date) lst)))
