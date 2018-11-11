;; -*- origami-fold-style: triple-braces; -*-
;; Port CSS to css-expr
#lang racket
(require css-expr)
;; Font Definitions {{{
(define cjk-fallback
  '("Noto Sans CJK TC"
    "Microsoft Jhenghei"
    "Microsoft Yahei"
    "Meiryo"
    "Malgun Gothic"))
(define sans-serif
  `("Fira Sans"
    ,@cjk-fallback
    "sans-serif"))
(define title-sans-serif
  `("Overpass"
    ,@cjk-fallback
    "sans-serif"))
(define monospace
  `("Overpass Mono"
    "Noto Sans Mono CJK"
    ,@cjk-fallback
    "monospace"))
;;; }}}

;;; CSS {{{
(css-expr->css
 (css-expr
  [body
   #:line-weight 1.6
   #:font-family ,@sans-serif
   #:font-weight 400
   #:color \#444
   #:text-rendering optimizeLegibility]
  [footer
   #:margin-top 2em]
  [html
   body
   #:background-color \#fbfbfb]
  [(\#logo img)
   #:max-height 2em
   #:margin-right 0.4em]
  [a
   #:color \#a868e8
   #:-webkit-transition (all 0.4s ease)
   #:-moz-transition: (all 0.4s ease)
   #:-o-transition: (all 0.4s ease)
   #:-ms-transition: (all 0.4s ease)
   #:transition: (all 0.4s ease)]
  [a:hover
   #:color \#d0a3ff
   #:text-decoration none]
  [.index-item
   #:margin-top 0.5rem
   #:margin-bottom 0.5rem]
  [(.index-header .title)
   (.post-header .title)
   #:font-size 1.5rem
   #:line-height 1.6
   #:margin-top 0]
  [.text-primary
   #:color \#333 !important]))

;;; }}}
