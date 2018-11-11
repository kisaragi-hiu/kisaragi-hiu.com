#lang rackjure
(require txexpr
         threading
         "define-txexpr.rkt"
         pollen/template/html)

(provide (all-defined-out))

(define global-translation-dict
  {"Kisaragi Hiu"
   {'ja "如月飛羽"
    'zh "如月飛羽"
    'en "Kisaragi Hiu"}
   "Contact:"
   {'ja "連絡："
    'zh "聯絡："
    'en "Contact:"}})

(define (translate #:translation-dict [translation-dict global-translation-dict]
                   .
                   strings)
  (define str (string-join strings ""))
  (~> (translation-dict str) 'en))

;; ◊$[#:en "Eng" #:zh "華"] ->
;; '(span ([class "translate"])
;;   (span ([class "lang-en"]) "Eng")
;;   (span ([class "lang-zh"]) "華"))
(define translate-inline
  (make-keyword-procedure
   (lambda (kws kw-args . rest)
     (~> (map (lambda (kw kw-arg)
                `(span
                  ([class ,(~a "lang-" (keyword->string kw))])
                  ,kw-arg))
              kws kw-args)
         (cons 'span _)
         (attr-set _ 'class "translate")
         ;; There's no statisfying way to pass this in with a keyword.
         ;; Use parameterize instead.
         ((if (current-return-txexpr?) identity ->html) _)))))
