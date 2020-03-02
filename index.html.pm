#lang pollen
◊define-meta[title]{Home}
◊define-meta[type]{root-index}

◊(current-pagetree "index.ptree")

◊(define blog-entries (children 'blog "index.ptree"))
◊(define blog-entry-years
   (remove-duplicates (map post-year blog-entries)))

Just a random person on the internet.

Stuff I've made:

◊ul{
◊li{◊link["https://gitlab.com/canrylog/canrylog.el"]{Canrylog (WIP time tracking app)}}
◊li{◊link["https://barren-moon.kisaragi-hiu.com"]{A Barren Land of Dust (Poem collection from my high school years)}}
◊li{UTAU Covers (◊youtube["channel/UCl_hsqcvdX0XdgBimRQ6R3A"]{on Youtube}, ◊link["https://www.nicovideo.jp/user/38995186"]{on niconico})}
}

◊link["/projects.html"]{Other projects...}

◊for/splice[((year blog-entry-years))]{
  ◊div[#:class "index-year"]{
    ◊heading[(number->string year)]
    ◊(txexpr 'div '([class "index"])
             (map (lambda (entry) (index-item entry #:year? #f))
                  (filter (curryr post-year=? year) blog-entries)))}}
