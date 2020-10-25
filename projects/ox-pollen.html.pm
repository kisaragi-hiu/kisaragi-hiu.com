#lang pollen

◊define-meta[type]{page}
◊define-meta[title]{ox-pollen}
◊heading{ox-pollen}

ox-pollen allows exporting Org mode to Pollen markup.

The Pollen markup it generates is somewhat opinionated, simply because of how flexible Pollen markup is.

It is to convert Org mode keywords (◊code{#+title:} and the like) to ◊code{define-meta} statements. Combined with Make, it makes for a pretty nice workflow for writing pages in Org in a Pollen project.

Repository: ◊link["https://github.com/kisaragi-hiu/ox-pollen"]
