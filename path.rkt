#lang racket

(provide abs-local abs-global path-from-project-root)

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
