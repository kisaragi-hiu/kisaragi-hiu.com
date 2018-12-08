#lang racket
(require threading
         txexpr
         pollen/core
         pollen/template
         pollen/decode
         pollen/pagetree
         pollen/tag
         pollen/unstable/pygments
         "widgets.rkt"
         (for-syntax racket/string
                     threading))

(provide (all-defined-out)
         highlight
         (all-from-out "widgets.rkt"
                       pollen/pagetree))

(define (root . elements)
  (txexpr 'root '() (decode-elements
                     elements
                     #:txexpr-elements-proc decode-paragraphs
                     #:exclude-tags '(pre))))

(define-syntax (->2to-define stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax
       ([new-name
         (datum->syntax
          #'name
          (~> (syntax->datum #'name)
              symbol->string
              (string-replace _ "->" "-to-")
              (string-replace _ #rx"^-" "")
              string->symbol))])
       #'(define new-name name))]))

(->2to-define ->html)
