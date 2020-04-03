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

;; Create a pagetree leaf of html.pm files under DIRECTORY.
;; NODE is the leaf's tag.
;;
;; e.g. we have a.html.pm and b.html.pm under projects/;
;; calling `leaf-from-dir 'xyz "projects"` returns
;; '(xyz projects/a.html projects/b.html)
(define (leaf-from-dir node directory)
  (~>> (directory-list directory #:build? #t)
       (map path->string)
       (filter (curryr string-suffix? ".html.pm"))
       (map (curryr string-replace ".pm" ""))
       (map string->symbol)
       (cons node)))
