#lang racket/base
(require racket/port
         racket/list
         net/url
         json
         threading)

(provide youtube/formatted-title)

(define (get-json url)
   (call/input-url (string->url url)
                   get-pure-port
                   port->string
                   string->jsexpr))

(define (youtube/formatted-title id)
  (define key (getenv "YOUTUBE_API_KEY"))
  (define vid-info
    (~> (get-json
          (string-append "https://www.googleapis.com/youtube/v3/videos?part=id%2C+snippet&id=" id
                         "&key=" key))
        (hash-ref _ 'items)
        first
        (hash-ref _ 'snippet)))
  (define title (~> vid-info
                    (hash-ref _ 'localized)
                    (hash-ref _ 'title)))
  (define date (~> vid-info
                   (hash-ref _ 'publishedAt)))
  (string-append date title))
