#lang pollen

◊define-meta[title]{org-msr}
◊heading{Org-msr}

Org-msr is a minimal spaced repetition setup for Org mode.

◊github["kisaragi-hiu/org-msr"]{Github}

◊gitlab["kisaragi-hiu/org-msr"]{Gitlab}

◊subheading{Usage}

Run ◊code{org-msr-setup} in a Org mode file to
◊ul{
◊li{add file TODO keywords according to ◊code{org-msr-keyword-frequency-alist}}
◊li{start ◊code{org-msr-mode} automatically when visiting this file.}
}

Now for each heading you want to remember, run ◊code{org-todo} and select how frequently you want to see it. A repeater will automatically be added and updated, and you can see all items in your org-agenda.

I recommend using ◊link["https://github.com/alphapapa/org-super-agenda"]{org-super-agenda} and a ◊link["https://orgmode.org/manual/In_002dbuffer-settings.html"]{filetag} so that the ◊code{org-msr} entries don’t bury everything else in the agenda.

In your init:
◊highlight['elisp]{
(setq org-super-agenda-groups '((:name "Vocabulary"
                                       :tag "org-msr"
                                       :order 100)))
}

In the Org file:
◊highlight['org]{
#+FILETAGS: :org-msr:
}
