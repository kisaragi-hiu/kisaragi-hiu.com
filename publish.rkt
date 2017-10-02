#lang racket
; Really just moving a bunch a files
(require threading
         shell/pipeline)

(let* ([CWD (path->string (current-directory))]
       [files (~> (run-pipeline/out `(find ,CWD
                                           -name *.html
                                           -or -name *.css
                                           -or -name *.js
                                           -not -regex .*/public/.*))
                  (string-split _ "\n"))]
       [new-files (map (lambda (entry)
                         (string-replace entry
                                         CWD
                                         (string-append CWD
                                                        "public/")))
                       files)])
  (map (lambda (source target)
         (run-pipeline `(mkdir ,(~> target
                                    string->path
                                    path-only
                                    path->string))
                       `(mv ,source ,target)))
       files
       new-files))

(let* ([CWD (path->string (current-directory))]
       [files (~> (run-pipeline/out `(find ,CWD
                                           -name feed.xml
                                           -or -name favicon.ico
                                           -or -name README.md
                                           -or -name LICENSE
                                           -or -name CC-BY-ND.txt
                                           -or -regex .*/images/.*
                                           -not -regex .*/public/.*))
                  (string-split _ "\n"))]
       [new-files (map (lambda (entry)
                         (string-replace entry
                                         CWD
                                         (string-append CWD
                                                        "public/")))
                       files)])
  (map (lambda (source target)
         (run-pipeline `(mkdir ,(~> target
                                    string->path
                                    path-only
                                    path->string))
                       `(cp ,source ,target)))
       files
       new-files))
