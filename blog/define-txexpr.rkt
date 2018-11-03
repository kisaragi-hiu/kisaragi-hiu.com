#lang racket

(require pollen/template/html)

(provide (all-defined-out))

(define current-return-txexpr? (make-parameter #f))

(define-syntax (define/txexpr stx)
  (syntax-case stx ()
    [(_ (name args ... . rest) body ... last-body)
     #'(begin
         (define (name #:return-txexpr? [return-txexpr? (current-return-txexpr?)] args ... . rest)
           body ...
           ((if return-txexpr? identity ->html) last-body)))]))
