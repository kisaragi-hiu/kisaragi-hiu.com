;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((eval . (setq org-attach-id-dir (expand-file-name "static/" (project-root (project-current)))))
              (minaduki:link-insertion-format . relative))))
