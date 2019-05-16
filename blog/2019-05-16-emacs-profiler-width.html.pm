#lang pollen
◊define-meta[title]{Setting the width of Emacs profiler reports}
◊define-meta[date]{2019-05-16T21:39:41}
◊define-meta[category]{tutorials}
◊define-meta[language]{en}

◊tldr{◊link["https://www.gnu.org/software/emacs/manual/html_node/elisp/Setting-Generalized-Variables.html"]{◊code{setf}} the caar of ◊code{profiler-report-cpu-line-format} and ◊code{profiler-report-memory-line-format} to a larger width (default 50 and 55).}

Emacs's profiler reports have a fixed width, which makes debugging deeply nested function calls ◊link["https://emacs.stackexchange.com/questions/7344/make-profiler-report-columns-wider"]{difficult}.

◊image["/static/emacs-26.2-profiler.png"]{truncated at column 29}

Ideally the columns would be resized along with the window, or perhaps ◊emacs-source[#:branch "emacs-26" #:file "lisp/profiler.el"]{profiler.el} should define its formatting variables with ◊code{defcustom}. Either way, it is still relatively easy to change the format.

The formatting is stored in two variables, ◊code{profiler-report-cpu-line-format} and ◊code{profiler-report-memory-line-format}. They're not documented, but through a bit of guesswork it happens that their ◊code{caar} is the width value.

In ◊emacs-source[#:branch "emacs-26" #:file "lisp/profiler.el" #:line "446"]{profiler.el} (comments added):

◊highlight['elisp]{
(defvar profiler-report-cpu-line-format
  ;; ↓ this
  '((50 left)
    (24 right ((19 right)
	       (5 right)))))

(defvar profiler-report-memory-line-format
  ;; ↓ this
  '((55 left)
    (19 right ((14 right profiler-format-number)
	       (5 right)))))
}

To change the width of lines in the profiler report, simply set these to what you want.

◊highlight['elisp]{
(setf (caar profiler-report-cpu-line-format) 80
      (caar profiler-report-memory-line-format) 80)
}

And here is a minor mode to do it:

◊highlight['elisp]{
(require 'profiler)
(define-minor-mode kisaragi/profiler-wide-mode
  "Minor mode to widen profiler reports."
  :global t
  (if kisaragi/profiler-wide-mode
      (setf (caar profiler-report-cpu-line-format) 80
            (caar profiler-report-memory-line-format) 80)
    (setf (caar profiler-report-cpu-line-format) 50
          (caar profiler-report-memory-line-format) 55)))
}

◊image["/static/emacs-26.2-profiler-width-80.png"]{not truncated even at column 37}

I'm sure there are ways to make the width update when Emacs is resized, but IMO that should be implemented inside ◊code{profiler.el} itself, not as an extension. For now, this is a good enough workaround for me.

This is a solution to ◊link["https://emacs.stackexchange.com/questions/7344/make-profiler-report-columns-wider"]{this StackExchange question}. Specifically, a comment there pointed out that ◊code{profiler-report} mentions "width", which led me to find the relevant variables.
