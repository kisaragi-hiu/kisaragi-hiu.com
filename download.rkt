#lang racket/base
(require net/url
         racket/file
         racket/format
         racket/function
         racket/port
         racket/path
         racket/system)

(provide download)

(define (download url-string path)
  (with-handlers ([exn:fail:filesystem?
                   (thunk* (displayln "file exists, skipping download"))])
    ;; Ensure path exists.
    (make-directory* (path-only path))

    (with-output-to-file path
      (thunk
       (display
        (port->string (get-pure-port (string->url url-string))))))))
