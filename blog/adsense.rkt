#lang racket/base

(require pollen/template)

(provide (all-defined-out))

(define (google-adsense/page-level id)
  (string-append
    "<script async src=\"//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>\n"
    "<script>\n"
    "(adsbygoogle = window.adsbygoogle || []).push({\n"
    "  google_ad_client: " id ",\n"
    "  enable_page_level_ads: true\n"
    "});\n"
    "</script>"))

(define google-adsense/banner
  (->html
   '((script ([async "async"]
              [src "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"]))
     (ins ([class "adsbygoogle"]
           [data-ad-client "ca-pub-6215394828182929"]
           [data-ad-slot "7498976067"]
           [style "display:inline-block;width:728px;height:90px"]))
     (script "(adsbygoogle = window.adsbygoogle || []).push({});"))))

(define google-adsense/in-article
  (->html
   '((script ([async "async"]
              [src "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"]))
     (ins ([class "adsbygoogle"]
           [style "display:block;text-align:center"]
           [data-ad-layout "in-article"]
           [data-ad-format "fluid"]
           [data-ad-client "ca-pub-6215394828182929"]
           [data-ad-slot "5614709443"]))
     (script "(adsbygoogle = window.adsbygoogle || []).push({});"))))

(define google-adsense/responsive
  (->html
   '((script ([async "async"]
              [src "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"]))
     (ins ([class "adsbygoogle"]
           [style "display:block"]
           [data-ad-client "ca-pub-6215394828182929"]
           [data-ad-slot "5733348692"]
           [data-ad-format "auto"]))
     (script "(adsbygoogle = window.adsbygoogle || []).push({});"))))

(define (google-universal-analytics account)
  ;; copied from https://github.com/greghendershott/frog/blob/master/frog/widgets.rkt
  ;; to avoid 'type="text/javascript"'
  (->html
    `(script ,(string-append
                "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
                "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
                "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
                "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"

                "ga('create', '" account "', 'auto');"
                "ga('send', 'pageview');"))))
