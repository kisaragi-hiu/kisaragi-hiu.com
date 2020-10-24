#lang pollen
◊define-meta[title]{Getting started with Magit (Or what I hope I could've read when I first tried out Magit)}
◊define-meta[date]{2018-09-20T23:57:41+0900}
◊define-meta[language]{en}
◊define-meta[category]{Tutorials}
◊define-meta[toc #t]

This is a quick guide for using ◊link["https://magit.vc/"]{Magit}, a wonderful Git interface based in Emacs. Its base features are fairly simple, but it can be confusing if you're not already using Emacs, as I was a few months ago.

◊video/gif-esque["/static/emacs-magit-typical-workflow.mp4" #:controls? #t]{Workflow as I edit Cangjie.el}

◊heading{The ◊code{magit} command}

I'll explain Emacs basics in the last paragraph.

The stage / committing workflow with Magit is centered around an interactive version of ◊code{git status}.

Call ◊code{magit} in a git repository, ie. when you have a file or folder open in the repo. The window that pops up is that interactive ◊code{git status}.

◊video/gif-esque["/static/emacs-magit-status.mp4"]{5 second screencast of M-x magit RET}

Move the cursor onto a hunk and press ◊kbd["Tab"]. This toggles display of its contents. Press ◊kbd["s"] to stage, ◊kbd["u"] to unstage, ◊kbd["c"] for commit options, ◊kbd["F"] for pulling options, and ◊kbd["P"] for pushing options.

This alone, I feel, is many times faster than using ◊code{git status}, ◊code{git diff}, ◊code{git add}, ◊code{git commit}, etc. directly.

Pressing ◊kbd["Enter"] will visit the hunk under cursor.

Also check out other options in the "Magit" menu located in the menu bar, when you're in the Magit window.

◊video/gif-esque["/static/emacs-magit-quick-workflow.mp4"]{15 seconds of quick showcase}

◊heading{Setting up Magit (and Emacs)}

If you have an Emacs configuration already, just install Magit from MELPA and ignore this.

This will introduce Emacs package management with ◊code{use-package}, in a way that I think is easier to maintain.

Edit the file ◊code{~/.emacs.d/init.el}, then type this into it:

◊highlight['elisp]{
(require 'package)
;; load packages ourselves
;; press C-h v on the variable for more documentation
(setq package-enable-at-startup nil)
;; Add MELPA to package-archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; load packages now
(package-initialize)

;; Emacs adds `custom' settings in the init file by default. Run this file
;; without this segment to see what that means.
;; Put those away in "custom.el".
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :noerror)

;; update local database then install use-package if it's not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; tell use-package to install a package if it's not already installed
(setq use-package-always-ensure t)
}

Then install Magit and set a keyboard shortcut to invoke it:

◊highlight['elisp]{
(use-package magit
  :bind (("C-x g" . magit)))
}

Read more about Magit in its manual. ◊link{https://magit.vc/manual/magit/}

◊heading{Emacs basics}

The mouse can be used for selection or moving the cursor around, like other editors. Typing something with an active selected region by default does not replace the text; to enable the expected behavior, add ◊code{(delete-selection-mode 1)} to your init file. A selected region is just called a "region" in Emacs.

Arrow keys work as expected, and you can explore other more efficient movement keys in the Emacs tutorial; start it with ◊kbd["C-h t"], or run ◊kbd["M-x"] ◊code{help-with-tutorial}.
