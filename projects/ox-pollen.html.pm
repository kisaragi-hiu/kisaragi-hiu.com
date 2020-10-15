#lang pollen

◊define-meta[type]{page}
◊define-meta[title]{ox-pollen}
◊heading{ox-pollen}

ox-pollen allows exporting Org mode to Pollen markup. At least when it's done.

The Pollen markup it generates will be somewhat opinionated, simply because of how flexible Pollen markup is.

It will be able to convert Org mode keywords (◊code{#+title:} and the like) to ◊code{define-meta} statements, and combined with Make will hopefully allow editing a Pollen site in Org.

Repository: ◊link["https://github.com/kisaragi-hiu/ox-pollen"]
