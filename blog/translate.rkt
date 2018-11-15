#lang rackjure
(require txexpr
         threading
         "define-txexpr.rkt"
         pollen/template/html)

(provide (rename-out [translate !])
         (rename-out [translate-inline $]))

(define global-translation-dict
  ;; TODO: automatically transform ({'en "abc"}) into {"abc" {'en "abc"}}
  {"Kisaragi Hiu"
   {'ja "如月飛羽"
    'zh "如月飛羽"
    'en "Kisaragi Hiu"}
   "Contact:"
   {'ja "連絡："
    'zh "聯絡："
    'en "Contact:"}
   "Tag"
   {'ja "タグ"
    'zh "標籤"
    'en "Tag"}})

(define/txexpr (translate #:translation-dict [translation-dict global-translation-dict]
                   .
                   strings)
  ;; dictionary representing this string's translations
  (define this-dict
    (~> (dict-ref translation-dict (string-join strings ""))
        dict->list ; ensure it's an alist
        (sort _ (lambda (x y) (symbol<? (car x) (car y))))))

  (define kws (~>> (dict-keys this-dict)
                   (map (compose1 string->keyword symbol->string))))
  (define kw-args (dict-values this-dict))
  (parameterize ([current-return-txexpr? #t])
    (keyword-apply translate-inline kws kw-args empty)))

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
