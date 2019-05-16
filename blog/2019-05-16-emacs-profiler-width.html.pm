#lang pollen
◊define-meta[title]{Setting the width of Emacs profiler reports}
◊define-meta[date]{2019-05-16T21:39:41}
◊define-meta[category]{tutorials}
◊define-meta[language]{en}

◊tldr{◊code{setf} the caar of ◊code{profiler-report-cpu-line-format} and ◊code{profiler-report-memory-line-format} to 100.}

Emacs's profiler reports have a fixed width, which makes debugging deeply nested function calls ◊link["https://emacs.stackexchange.com/questions/7344/make-profiler-report-columns-wider"]{difficult}.

◊image["/static/emacs-26.2-profiler.png"]

Ideally the columns would be resized along with the window, or perhaps ◊emacs-source[#:branch "emacs-26" #:file "lisp/profiler.el"]{profiler.el} should define its formatting variables with ◊code{defcustom}. Either way, it is still relatively easy to change the format.

Here is a minor mode to do it:

◊highlight['elisp]{
(require 'profiler)
(define-minor-mode kisaragi/profiler-wide-mode
  "Minor mode to widen profiler reports."
  :global t
  (if kisaragi/profiler-wide-mode
      (setf (caar profiler-report-cpu-line-format) 100
            (caar profiler-report-memory-line-format) 100)
    (setf (caar profiler-report-cpu-line-format) 50
          (caar profiler-report-memory-line-format) 55)))
}

This relies on how ◊emacs-source[#:branch "emacs-26" #:file "lisp/profiler.el" #:line "74"]{profiler-format} refers to the two variables, ◊code{profiler-report-cpu-line-format} and ◊code{profiler-report-memory-line-format}, for its formatting. I found this tip on ◊link["https://emacs.stackexchange.com/questions/7344/make-profiler-report-columns-wider"]{StackExchange}.
