#lang racket

(require pollen/core
         threading)

(provide post-in-category?
         post-has-tag?
         post-date
         post-date<?
         post-date>?
         post-year
         post-year=?
         post-type
         post-category
         post-tags)

;; get type of current post
(define (post-type metas)
  (or (select-from-metas 'type metas)
      "post"))

;; get timestamp of pagenode as specified in its source file
(define (post-date pagenode)
  (select-from-metas 'date pagenode))

;; is pagenode-a posted earlier than pagenode-b?
;;
;; not caring about timezones for now, because I haven't added them
;; plus we don't care about the time that much
(define (post-date<? pagenode-a pagenode-b)
  (string<? (post-date pagenode-a)
            (post-date pagenode-b)))

;; opposite of above
(define (post-date>? pagenode-a pagenode-b)
  (string>? (post-date pagenode-a)
            (post-date pagenode-b)))

;; get year of
(define (post-year pagenode)
  (~> (post-date pagenode)
      (substring 0 4)
      string->number))

(define (post-year=? pagenode year)
  (= year (post-year pagenode)))

(define (post-category pagenode)
  (select-from-metas 'category pagenode))

(define (post-tags pagenode)
  (select-from-metas 'tags pagenode))

(define (post-in-category? pagenode category)
  (define cat (post-category pagenode))
  (and (string? cat)
       (string-ci=? cat category)))

(define/contract (post-has-tag? pagenode tag)
  (-> symbol? string? any/c)
  (member tag (post-tags pagenode) string-ci=?))
