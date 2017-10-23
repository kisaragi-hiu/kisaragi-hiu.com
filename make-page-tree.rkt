#lang racket

(require pollen/core
         pollen/pagetree
         pollen/render
         pollen/template
         racket/date
         rackjure
         rackunit
         shell/pipeline
         txexpr
         "./_common/date.rkt")

(define tag-dir "category/")
(define lang-dir "language/")

(define (make-index title)
  (define out (open-output-file (string-append title ".html.pm")
                                #:mode 'binary
                                #:exists 'replace))
  (display (string-append "#lang pollen/markup
◊define-meta[template]{index-template.html}
◊define-meta[title]{" title "}") out)
  (close-output-port out))

(define (make-tag-index title)
  (define out (open-output-file (string-append tag-dir title ".html.pm")
                                #:mode 'binary
                                #:exists 'replace))
  (display (string-append "#lang pollen/markup
◊define-meta[template]{tag-template.html}
◊define-meta[title]{" title "}") out)
  (close-output-port out))

(define (make-lang-index title)
  (define out (open-output-file (string-append lang-dir title ".html.pm")
                                #:mode 'binary
                                #:exists 'replace))
  (display (string-append "#lang pollen/markup
◊define-meta[template]{lang-template.html}
◊define-meta[title]{" title "}") out)
  (close-output-port out))

(define (list-pms directory)
  ; dirty, sure
  ; remember regex n* means 0 or more n
  ; so pmd* matches pmd.. as well as pm
  (~> (run-pipeline/out `(find ,directory -maxdepth 1 -regex .*\.pmd*))
      (string-split _ "\n")
      (map (λ (x) (string-replace x #rx"^./" "")) _) ; strip "./" away
      (filter (λ (x) (not (equal? x "index.html.pm"))) _) ; "don't include index.html"
      (map string->path _)))

(define (list-pps directory)
  (~> (run-pipeline/out `(find ,directory -maxdepth 1 -regex .*\.pp*))
      (string-split _ "\n")
      (map (λ (x) (string-replace x #rx"^./" "")) _) ; strip "./" away
      (filter (λ (x) (not (string-contains? x "template"))) _) ; don't include templates
      (map string->path _)))

(define (pm->html file)
  (string-trim (path->string file)
               #rx".pmd*"
               #:left? #f #:repeat? #f))

(define (pp->html file)
  (string-trim (path->string file)
               #rx".pp*"
               #:left? #f #:repeat? #f))

(define (pm->html-symbol file)
  (string->symbol (string-trim (path->string file) #rx".pmd*"
                               #:left? #f #:repeat? #f)))

(define (cat-string->list string)
  (map (λ (tag)
         (apply string-append tag))
       (map (λ (tag)
              (add-between tag " "))
            (map string-split (map string-trim (string-split string ","))))))

(define (tag-in-file? tag file)
  (if (select-from-metas 'categories file)
      (findf (λ (x)
               (equal? x tag))
             (cat-string->list (select-from-metas 'categories file)))
      #f))

#|
find-tags: find categories metadata in all files
|#
(define (find-tags files)
  (~> (map (λ (file)
             (if (select-from-metas 'categories file)
                 (cat-string->list (select-from-metas 'categories file))
                 #f))
           files)
      ((λ (x) (filter identity x))) ; remove #f
      flatten
      remove-duplicates))

(define (find-languages files)
  (~> (map (λ (file)
             (or (select-from-metas 'language file)
                 "zh-tw")) ; use zh-tw by default
           files)
      ((λ (x) (filter identity x))) ; remove #f
      flatten
      remove-duplicates))

;; Extract all categories from files
;; For each category find all files that are tagged with it
;; Generate index page with those files
(define (generate-cats tags)
  (apply string-append (map (λ (tag)
                             (make-tag-index tag)
                             (string-append tag-dir tag ".html\n"))
                        tags)))

(define (generate-langs langs)
  (apply string-append (map (λ (lang)
                             (make-lang-index lang)
                             (string-append lang-dir lang ".html\n"))
                        langs)))

(define (make-.ptree #:post post-pages #:nonpost nonpost-pages
                     #:js js-files #:css css-files
                     #:tags tags #:languages languages)
  (define out (open-output-file "index.ptree" #:exists 'replace))
  (define st
    (string-append "#lang pollen\n◊index.html{"
       (apply string-append
              (map (λ (x)
                     (string-append (pm->html x) "\n")) post-pages))
       "}\n"
       (apply string-append
              (map (λ (x)
                     (string-append (pm->html x) "\n")) nonpost-pages))
       "◊category/index.html{"
       (generate-cats tags)
       "}\n"
       "◊language/index.html{"
       (generate-langs languages)
       "}\n"
       (apply string-append
              (map (λ (x)
                     (string-append (pp->html x) "\n")) js-files))
       (apply string-append
              (map (λ (x)
                     (string-append (pp->html x) "\n")) css-files))))
  (display st out)
  (close-output-port out))

(define post-pages (order-by-date (list-pms "./post/")))
(define tags (find-tags post-pages))
(define languages (find-languages post-pages))
(define nonpost-pages (list-pms "./"))
(define js-files (list-pps "./js/"))
(define css-files (list-pps "./css/"))

(make-.ptree #:post post-pages
             #:nonpost nonpost-pages
             #:js js-files
             #:css css-files
             #:tags tags
             #:languages languages)
