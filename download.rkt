#lang racket/base
(require net/url
         racket/file
         racket/format
         racket/function
         racket/port
         racket/system)

(provide download)

(define (download url path)
  (with-handlers ([exn:fail:filesystem?
                   (thunk* (displayln "file exists, skipping download"))])
    ;; Ensure path exists.
    ;; FIXME: how do I get a simple dirname function without shelling out?
    (make-directory* (system (~a "dirname " path)))

    (with-output-to-file path
      (thunk
       (display
        (port->string (get-pure-port url)))))))
