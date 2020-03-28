#lang racket
;; miscellaneous utilities

(require threading)

(provide (all-defined-out))

;; '("abc" (b "def")) -> "abcdef"
(define (extract-xexpr-strings xexpr)
  (if (list? xexpr)
      (~> (flatten xexpr) (filter string? _) (string-join _ ""))
      xexpr))

(define (ensure-timezone str)
  ;; default to +8 timezone
  (if (regexp-match? #rx"\\+|Z" str)
      str
      (~a str "+08:00")))
