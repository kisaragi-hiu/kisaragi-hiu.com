#lang pollen

◊define-meta[type]{page}
◊define-meta[title]{ust-mode}
◊heading{ust-mode}

ust-mode is an Emacs major mode for editing UTAU projects (UST files). It is hosted ◊github["kisaragi-hiu/org-msr"]{on Github}.

◊image["/static/emacs-ust-mode.png" "Editing a UST"]

This major mode currently provides syntax highlighting (based on ◊code{conf-mode}) and a command to normalize paths like OutFile and CacheDir.
