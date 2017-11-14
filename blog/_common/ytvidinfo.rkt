#lang racket/base
(require racket/port
         racket/list
         racket/string
         pollen/template/html
         net/url
         json
         threading)

(provide youtube/vidinfo
         youtube/vidlist)

(define (get-json url)
   (call/input-url (string->url url)
                   get-pure-port
                   (compose string->jsexpr port->string)))

; youtube/vidinfo : String -> (ListOf String)
(define (youtube/vidinfo id)
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
                   (hash-ref _ 'publishedAt)
                   (string-split _ "T") first
                   (string-replace _ "-" "/")))
  (define url (string-append "https://youtu.be/" id))
  `(span ([class "youtube-vid-info"])
         (p ,date)
         (a ([href ,url]
             [target "_blank"])
            ,title)))

; youtube/formatted-title : (ListOf String) -> String
(define (youtube/vidlist ids)
  (~> (map youtube/vidinfo ids)
      ->html))
