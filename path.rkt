#lang racket
(require threading)

(provide abs-local
         abs-global
         path-from-project-root
         slug
         urn)

(define current-project-root (make-parameter (current-directory)))
(define current-site-prefix  (make-parameter "/"))
(define current-site-host    (make-parameter "https://kisaragi-hiu.com/"))

;; append local site prefix
(define (abs-local . rest)
  (apply ~a (current-site-prefix) rest))

;; append global site prefix
(define (abs-global . rest)
  (apply ~a (current-site-host) rest))

;; return a site build-time file system path-string
(define (path-from-project-root . rest)
  (apply ~a (current-project-root) rest))

;; derived from frog: master:frog/paths.rkt:283
(define (slug s)
  (~>
   ;; First normalize string to Unicode composite form, so e.g. á will
   ;; be a single char for which char-alphabetic? is true. (In the
   ;; decomposed form á would be a plain a char followed by an accent
   ;; char, and the latter is not char-alphabetic? and would get
   ;; slugged to a hyphen.)
   (for/list ([c (in-string (string-normalize-nfc s))])
     (cond [(or (char-alphabetic? c)
                (char-numeric? c)) c]
           [else #\-]))
   list->string
   ;; Only one consecutive hyphen
   (regexp-replace* #px"-{2,}"  _ "-")
   ;; No trailing hyphen
   (regexp-replace  #px"-{1,}$" _ "")
   ;; Finally normalize to decomposed form. The rationale is that if
   ;; you use this result in a filename it will (hopefully) be
   ;; consistent across filesystems like Linux vs macOS.
   string-normalize-nfd))

;; from frog: master:frog/private/feeds.rkt:272
(define (urn uri-path)
  ;; Note that URNs have a more restricted syntax than URIs. Here we
  ;; just rely on `slug` to comply.
  (~a "urn:" (slug (abs-global))
      ":" (slug uri-path)))
