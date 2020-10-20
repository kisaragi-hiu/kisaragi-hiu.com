#lang racket

(require threading
         pollen/file)

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
;;
;; pass a compare function to #:sort to sort the entries before
;; inserting them into the pagetree. The function is passed straight
;; to `sort`: it gets two arguments A and B, and returns non-nil if A
;; sorts before B. By default it applies string>?, taking care to
;; convert symbols to strings.
(define (leaf-from-dir node directory
                       #:suffix [suffix #f]
                       #:sort [sort-func
                               (lambda args
                                 (apply string>? (map symbol->string args)))])
  (~>> (directory-list directory #:build? #t)
       (map path->string)
       (filter
        (if suffix
            (curryr string-suffix? suffix)
            identity))
       (map ->output-path)
       (map path->string)
       (map string->symbol)
       (sort _ sort-func)
       (cons node)))
