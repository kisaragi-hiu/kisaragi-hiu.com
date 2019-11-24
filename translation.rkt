#lang racket

(require (for-syntax racket/syntax)
         txexpr)

(define-syntax (define-language+provide stx)
  (syntax-case stx ()
    [(_ name)
     ;; https://www.greghendershott.com/fear-of-macros/pattern-matching.html
     (with-syntax ([$name (format-id #'name "$~a" #'name)])
       #'(begin
           (define ($name elem)
             (if (txexpr? elem)
                 (txexpr (get-tag elem)
                         (dict-set (get-attrs elem)
                                   'lang (list (symbol->string 'name)))
                         (get-elements elem))
                 ($name (txexpr* 'span '() elem))))
           (provide $name)))]))

(define-language+provide en)
(define-language+provide zh)
(define-language+provide ja)
