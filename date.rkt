#lang racket/base

(require racket/format
         racket/list
         racket/string)

(provide (all-defined-out))

(define (iso8601-date-year iso8601)
  (first (string-split iso8601 "-")))

(define (iso8601-date-month iso8601)
  (second (string-split iso8601 "-")))

(define (iso8601-date->year-and-month-str iso8601)
  (define year (iso8601-date-year iso8601))
  (define month (iso8601-date-month iso8601))
  (~a year "/" month))
