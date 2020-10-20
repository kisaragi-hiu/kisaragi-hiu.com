#lang racket

(require threading
         pollen/file)

(provide (all-defined-out))

;; Create a pagetree leaf for each pollen source file under DIRECTORY.
;; NODE is the leaf's tag.
;;
;; e.g. we have a.html.pm and b.html.pmd under projects/;
;; calling `leaf-from-dir 'xyz "projects"` returns
;; '(xyz projects/a.html projects/b.html)
;;
;; Use eg. (leaf-from-dir 'node "dir" #:suffix ".xml.pm") to list
;; specifically XML files.
;;
;; pass a compare function to #:sort to sort the entries before
;; inserting them into the pagetree. The function is passed straight
;; to `sort`: it gets two string arguments A and B, and should return
;; non-nil if A sorts before B.
(define (leaf-from-dir node directory
                       #:suffix [suffix #f]
                       #:sort [sort-func
                               string>?])
  (~>> (directory-list directory #:build? #t)
       (map path->string)
       (filter
        (lambda (val)
          (or
           (markup-source? val)
           (markdown-source? val)
           (preproc-source? val)
           (null-source? val)
           (scribble-source? val)
           (pagetree-source? val))))
       (filter
        (if suffix
            (curryr string-suffix? suffix)
            identity))
       (map ->output-path)
       (map path->string)
       (sort _ sort-func)
       (map string->symbol)
       (cons node)))
