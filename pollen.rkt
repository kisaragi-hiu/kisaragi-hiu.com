#lang racket/base

(require racket/string
         racket/format)

(provide (all-defined-out))

(module+ setup
  (require racket/path racket/runtime-path)
  (provide (all-defined-out))
  (define-runtime-path rkt "rkt")
  (define cache-watchlist
    (filter
     (lambda (f)
       (and (path-has-extension? f ".rkt")
            (not (string-contains? (~a f) "flycheck"))
            (not (string-prefix? (~a f) "."))))
     (directory-list rkt #:build? #t))))
