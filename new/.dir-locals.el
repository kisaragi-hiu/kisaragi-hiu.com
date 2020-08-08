;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode
  (eval setq org-roam-directory
        (f-join
         (projectile-project-root)
         "new"))
  (org-link-abbrev-alist
   ("youtube" . "https://youtube.com/")
   ("niconico" . "https://nicovideo.jp/")
   ("twitter" . "https://twitter.com/")
   ("github" . "https://github.com/")
   ("gitlab" . "https://gitlab.com/"))))


