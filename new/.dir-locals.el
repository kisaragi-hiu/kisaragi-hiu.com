;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode
  (eval kisaragi/org-backlink-mode)
  (org-link-abbrev-alist
   ("youtube" . "https://youtube.com/")
   ("niconico" . "https://nicovideo.jp/")
   ("twitter" . "https://twitter.com/")
   ("github" . "https://github.com/")
   ("gitlab" . "https://gitlab.com/"))
  (eval add-to-list 'org-link-abbrev-alist (cons "site" (f-join (projectile-project-root) "new/")))))

