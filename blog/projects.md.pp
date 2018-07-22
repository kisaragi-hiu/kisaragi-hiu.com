#lang pollen
◊(require racket/string)
◊(define (code-project link description)
   (define repo (string-replace link #rx".*/" ""))
   (string-append "[" repo "](" link "): " description))

# Projects

◊image/link["//github.com/kisaragi-hiu/osuskin-retome"
            "//github.com/kisaragi-hiu/osuskin-retome/raw/master/screenshots/0.8/en/std-1.jpg"
            "osu! skin: Retome"]

- ◊code-project["//github.com/kisaragi-hiu/bk" "my personal key-value pair storage"]
- ◊code-project["//gitlab.com/kisaragi-hiu/randomwallpaper" "my personal wallpaper changer script"]
- ◊code-project["//gitlab.com/kisaragi-hiu/language-startup-benchmark" "time hello world in various languages to benchmark their startup times"]
- ◊code-project["//gitlab.com/kisaragi-hiu/scripts" "all my other scripts, like `fcitx-next-im`, `bopomofo`, `100andro-maintanence`, etc."]
