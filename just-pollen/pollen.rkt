#lang racket
(require threading
         pollen/core
         pollen/template
         pollen/decode
         pollen/pagetree
         pollen/tag
         pollen/unstable/pygments
         (for-syntax racket/string
                     threading))

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
