#lang racket/base
(require pollen/decode
         pollen/pagetree
         pollen/template
         pollen/unstable/pygments
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
         "rkt/widgets.rkt")


(provide (all-defined-out)
         highlight
         conf
         (all-from-out
          "rkt/config.rkt"
          "rkt/css.rkt"
          "rkt/misc.rkt"
          "rkt/path.rkt"
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

(define (root . elements)
  (if (txexpr-elements? elements)
      (txexpr 'root '() (decode-elements
                         elements
                         #:txexpr-elements-proc decode-paragraphs
                         #:exclude-tags '(pre blockquote)
                         #:exclude-attrs '((class "tweet"))))
      elements))

(define to-html ->html)
