#lang racket/base

(require racket/format
         racket/list
         racket/contract
         racket/string)

(provide (all-defined-out))

;;; Predicates

(define (iso8601-year? str)
  (and (string? str)
       (= (string-length str) 4)
       (<= 0 (string->number str) 9999)))

(define (iso8601-month? str)
  (and (string? str)
       (= (string-length str) 2)
       (<= 1 (string->number str) 12)))

(define (iso8601-day? str)
  ;; FIXME: doesn't catch 02-31
  (and (string? str)
       (= (string-length str) 2)
       (<= 1 (string->number str) 31)))

(define (iso8601-hour? str)
  (and (string? str)
       (= (string-length str) 2)
       (<= 0 (string->number str) 23)))

(define (iso8601-minute? str)
  (and (string? str)
       (= (string-length str) 2)
       (<= 0 (string->number str) 59)))

(define iso8601-second? iso8601-minute?)

(define (iso8601-zone? str)
  (and (string? str)
       (or (string=? str "Z")
           (and (= (string-length str) 6)
                (or (string-prefix? str "+")
                    (string-prefix? str "-"))
                (<= -12 (string->number (first (string-split str ":"))) 12)
                (<= 0 (string->number (second (string-split str ":"))) 59)))))

;;; Getters

(define (iso8601-year iso8601)
  (substring iso8601 0 4))

(define (iso8601-month iso8601)
  (second (string-split iso8601 "-")))

(define (iso8601-day iso8601)
  (third (string-split iso8601 "-")))

;;; Constructor

(define/contract (iso8601 year month day hour minute second
                          [zone "Z"])
  (->* (iso8601-year?
        iso8601-month?
        iso8601-day?
        iso8601-hour?
        iso8601-minute?
        iso8601-second?)
       (iso8601-zone?)
       string?)
  (format "~a-~a-~aT~a:~a:~a~a"
          year month day hour minute second zone))
