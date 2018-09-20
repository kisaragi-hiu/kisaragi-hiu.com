#lang pollen
    Title: Getting started with Magit
    Date: 2018-09-20T23:57:41
    Tags: language:en, category:Tutorials, Emacs, Magit

This is a quick guide for using Magit, a wonderful Git interface based in Emacs. Its base features are fairly simple, but it can be confusing if you're not already using Emacs, as I was a few months ago.

◊video/gif-esque["/videos/emacs-magit-typical-workflow.mp4"]{Workflow as I edit Cangjie.el}

## `magit`

I'll explain Emacs basics in the last paragraph.

The stage / committing workflow with Magit is centered around an interactive version of `git status`.

Call `magit` in a git repository, ie. when you have a file or folder open in the repo. The window that pops up is that interactive `git status`.

◊video/gif-esque["/videos/emacs-magit-status.mp4"]{5 second screencast of M-x magit RET}

Move the cursor onto a hunk and press ◊kbd["Tab"]. This toggles display of its contents. Press ◊kbd["s"] to stage, ◊kbd["u"] to unstage, ◊kbd["c"] for commit options, ◊kbd["F"] for pulling options, and ◊kbd["P"] for pushing options.

This alone, I feel, is many times faster than using `git status`, `git diff`, `git add`, `git commit`, etc. directly.

Pressing ◊kbd["Enter"] will visit the hunk under cursor.

Also check out other options in the "Magit" menu located in the menu bar, when you're in the Magit window.

◊video/gif-esque["/videos/emacs-magit-quick-workflow.mp4"]{15 seconds of quick showcase}

## Setting up Magit (and Emacs)

If you have an Emacs configuration already, just install Magit from MELPA and ignore this.

This will introduce Emacs package management with `use-package`, in a way that I think is easier to maintain.

Edit the file `~/.emacs.d/init.el`, then type this into it:

```lisp
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

(use-package magit)
```

## Emacs basics

The mouse can be used for selection or moving the cursor around, like other editors. Typing something with an active selected region by default does not replace the text; to enable the expected behavior, add `(delete-selection-mode 1)` to your init file. A selected region is just called a "region" in Emacs.

Arrow keys work as expected, and you can explore other more efficient movement keys in the Emacs tutorial; start it with ◊kbd["C-h t"], or run ◊kbd["M-x"] `help-with-tutorial`.
