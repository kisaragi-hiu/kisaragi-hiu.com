#lang racket/base

(require pollen/core
         racket/date
         racket/list
         racket/match
         racket/string
         threading)

(provide (all-defined-out))

(define (datestring->seconds datetime)
  ; datetime: "2017/09/22 [22:00]"
  (parameterize ([date-display-format 'chinese]) ; "2017/9/22 星期五"
    (match (string-split datetime)
      [(list date time) (match (map string->number (append (string-split date "/")
                                                           (string-split time ":")))
                          [(list year month day hour minutes) (find-seconds 0
                                                                            minutes
                                                                            hour
                                                                            day
                                                                            month
                                                                            year)])]
      [(list date) (match (map string->number (string-split date "/"))
                     [(list year month day) (find-seconds 0
                                                          0
                                                          0
                                                          day
                                                          month
                                                          year)])])))

(define (file-date-in-seconds file)
  (if (select-from-metas 'publish-date file)
      (datestring->seconds (select-from-metas 'publish-date file))
      0))

(define (order-by-date files)
  (sort files
        (λ (file1 file2)
          (> (file-date-in-seconds file1)
             (file-date-in-seconds file2)))
        #:cache-keys? #t))

(define (datestring->date datetime)
  ; datetime: "2017/09/22 [22:00]"
   (match (string-split datetime)
       [(list date time) (match (map string->number (append (string-split date "/")
                                                            (string-split time ":")))
                           [(list year month day hour minutes) (seconds->date
                                                                (find-seconds 0
                                                                              minutes
                                                                              hour
                                                                              day
                                                                              month
                                                                              year))])]
       [(list date) (match (map string->number (string-split date "/"))
                      [(list year month day) (seconds->date (find-seconds 0
                                                             0
                                                             0
                                                             day
                                                             month
                                                             year))])]))
(define (format-date string)
 (parameterize ([date-display-format 'chinese])
   (match (~> (datestring->date string)
              (date->string _)
              (string-split _ "/")
              (map string-split _)
              (flatten))
     [(list year month date day) ; day: 星期三, 星期五, etc.
      `(,year "年" ,month "月" ,date "日，" ,day)])))
