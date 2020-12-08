#lang racket/base
(require pollen/decode
         pollen/pagetree
         pollen/template
         pollen/setup
         pollen/unstable/pygments
         pollen/unstable/typography
         racket/format
         racket/function
         racket/list
         racket/string
         txexpr
         "rkt/config.rkt"
         "rkt/css.rkt"
         "rkt/misc.rkt"
         "rkt/path.rkt"
         "rkt/post.rkt"
         "rkt/reference.rkt"
         "rkt/widgets.rkt"
         "rkt/template.rkt")


(provide (all-defined-out)
         highlight
         conf
         (all-from-out
          "rkt/config.rkt"
          "rkt/css.rkt"
          "rkt/misc.rkt"
          "rkt/path.rkt"
          "rkt/template.rkt"
          "rkt/post.rkt"
          "rkt/reference.rkt"
          "rkt/widgets.rkt"
          pollen/pagetree
          pollen/template
          racket/format
          racket/function
          racket/list
          racket/string
          txexpr))

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

(define (root . elements)
  (if (txexpr-elements? elements)
      (txexpr 'root '() (decode-elements
                         elements
                         #:string-proc smart-dashes
                         #:txexpr-elements-proc decode-paragraphs
                         #:exclude-tags '(pre blockquote time)
                         #:exclude-attrs '((class "tweet"))))
      elements))

(define to-html ->html)
