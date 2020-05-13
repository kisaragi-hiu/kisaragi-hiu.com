#lang racket

(require threading)

(provide (all-defined-out))

;; Create a pagetree leaf of html.pm files under DIRECTORY.
;; NODE is the leaf's tag.
;;
;; e.g. we have a.html.pm and b.html.pm under projects/;
;; calling `leaf-from-dir 'xyz "projects"` returns
;; '(xyz projects/a.html projects/b.html)
;;
;; Use eg. (leaf-from-dir 'node "dir" #:suffix ".xml.pm") to list
;; non-html files.
(define (leaf-from-dir node directory #:suffix [suffix ".html.pm"])
  (~>> (directory-list directory #:build? #t)
       (map path->string)
       (filter (curryr string-suffix? ".html.pm"))
       (map (curryr string-replace ".pm" ""))
       (map string->symbol)
       (cons node)))
