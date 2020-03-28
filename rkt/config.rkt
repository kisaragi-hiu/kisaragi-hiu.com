#lang racket/base

(provide conf)

;; Configuration: stored in a hash table, use `conf` for easy access.
(define config
  #hash((author . "Kisaragi Hiu")
        (site-title . "Kisaragi Hiu")
        (desc . "")
        (keywords . "Coding, Programming, Language, LGBT, Blog")))
(define (conf key) (hash-ref config key))
