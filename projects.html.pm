#lang pollen
◊define-meta[type]{page}
◊define-meta[title]{Projects}
◊define-meta[toc #t]

A list of my projects.

Incomplete: UTAU covers, illustrations on Pixiv, other things that I've forgot, etc. still need to be added..

◊heading{Coding}

◊subheading{Emacs}

◊project["https://gitlab.com/kisaragi-hiu/kisaragi-hydration" "kisaragi-hydration"]{
My own hydration tracker.
}

◊project["/projects/ust-mode.html" "UST mode"]{
Major mode for UTAU project (UST) files.
}

◊project["https://gitlab.com/kisaragi-hiu/didyoumean.el" "DidYouMean.el"
         #:title2 ◊(melpa-badge "didyoumean")]{
Ask for the right file to open.

Emacs port of the ◊github["EinfachToll/DidYouMean"]{DidYouMean} Vim plugin.
}

◊project["https://gitlab.com/kisaragi-hiu/dired-show-readme" "dired-show-readme"]{
Dired extension to show README of current directory.
}

◊project["https://github.com/kisaragi-hiu/cangjie.el" "Cangjie.el"
         #:title2 ◊(melpa-badge "cangjie")]{
Retrieve Cangjie code for Han character in Emacs.

This is my first Emacs package.
}

◊project["https://gitlab.com/canrylog" "Canrylog"]{
An experimental time tracking application.
}

◊project["https://gitlab.com/kisaragi-hiu/yasearch" "yasearch"]{
Yet Another web Search plugin for Emacs.

Search the word under point or region on the web.
}

◊project["/projects/org-msr.html" "org-msr"]{
My personal spaced repetition system, implemented with Org-mode repeaters.
}

◊subheading{Web / Design}

◊project["https://github.com/kisaragi-hiu/kisaragi-hiu.com" "kisaragi-hiu.com"]{
This site. Built on Pollen and Racket, using a minimal style, and spamming purple everywhere.
}

◊project["https://gitlab.com/kisaragi-hiu/barren-moon" "如同月球般的荒涼(Barren Moon)"]{
A web-based book. Collection of poems I wrote in junior and senior high school.
}

◊subheading{Commands}

◊project["https://github.com/kisaragi-hiu/bk" "bk"]{
Key-value pair storage based on JSON, intended for command line bookmarks.
}

◊project["https://github.com/kisaragi-hiu/randomwallpaper" "randomwallpaper"]{
A script to set a random wallpaper from a directory every so often.
}

◊subheading{Other}

◊project["https://gitlab.com/kisaragi-hiu/language-startup-benchmark" "Language Startup Benchmark"]{
Time hello world in various languages to benchmark their startup times.
}

◊heading{osu! Skins & related}

◊project["https://github.com/kisaragi-hiu/osuskin-retome" "Retome"]{
My personal osu! skin, focused on glow and shadow effects.
}

◊project["https://github.com/kisaragi-hiu/font-rozerofo" "Rozerofo"]{
A font for the ◊em{Romoji} script from ◊link["https://en.wikipedia.org/wiki/Re:Zero_−_Starting_Life_in_Another_World"]{Re:Zero}.
}
