#lang pollen
◊define-meta[title]{Exporting my Slack data to an Org mode file}
◊define-meta[toc #t]
◊define-meta[date]{2020-02-16T03:19:38}
◊define-meta[tags ("Emacs" "Org")]
◊define-meta[language]{en}

◊heading{Story}

In 2017, I tried to use Slack as some sort of todo list, with channels serving as categories, messages being stuff to process, and myself being the only member. It did not work out, partly because I didn't have internet on my phone; I abandoned the personal Slack workspace shortly after.

My personal notes system has evolved a lot, and is now a central Git repository with a bunch of Org files in it. However, I never actually exported the data on the Slack workspace --- some notes were still only available there.

That's why I'd like to move them to my notes repository --- this only has to be done once.

First I tried to do it manually, checking the timestamp of each message, writing the text into a new Org file, categorized per channel. This is somewhat doable, as I only about 3 months worth of intermittent messages (in 20 channels) to move. Still, "checking the timestamp" involves hovering my cursor on the message and hoping it shows me the full timestamp --- I want to keep the seconds, as throwing away archival data feels wrong. Doing this quickly became tedious, so I started trying to work on the exported data direcly instead.

◊heading{Exporting the data from Slack}

◊image["/static/slack-to-workspace.png" "Workspace → Administration → Workspace settings"]
◊image["/static/slack-workspace-settings.png" "Import / Export Data"]
◊image["/static/slack-export.png" "Export → Choose \"Entire workspace history\""]

The data is in a zip file, structured like this:

◊highlight['tree]{
slack-export
├── a_channel
│   ├── 2017-02-14.json
│   └── 2017-03-02.json
├── another_channel
│   ├── 2017-02-14.json
│   ├── 2017-02-19.json
│   ├── 2017-02-21.json
│   └── 2017-03-02.json
├── channels.json
├── integration_logs.json
└── users.json
}

Messages from each channel is in its own folder. ◊path{channels.json} contains metadata for all channels, ◊path{users.json} contains all users in the workspace; I don't care about ◊path{integration_logs.json}, but it seems to be the installed Slack Apps.

◊heading{Starting to write it}

The code I wrote grew organically. Starting from:

◊highlight['elisp]{
(defun my/file->json (file)
  "Return contents of FILE read through `json-read'."
  (save-match-data
    (with-temp-buffer
      (insert-file-contents-literally file)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (json-read))))

(with-temp-file "slack.org"
  (insert "#+COLUMNS: %ITEM %CREATED %TOPIC %PURPOSE\n\n")
  (let ((channels (append (my/file->json "channels.json") nil)))
    (seq-doseq (channel channels)
      ;; Insert channel information
      (let-alist channel
        (insert "* =" .name "=\n")))))
}

I can then run ◊code{eval-buffer} to update ◊path{slack.org} for me to explore.

◊heading{The final logic}

◊subheading{The main script}

◊highlight['elisp]{
(with-temp-file "slack.org"
  (insert "#+COLUMNS: %ITEM %CREATED %TOPIC %PURPOSE\n\n")
  (let ((channels (append (my/file->json "channels.json") nil)))
    (seq-doseq (channel channels)
      ;; Insert channel information
      (let-alist channel
        (insert "* =" .name "=\n")
        (my/insert-properties
         `((created . ,(my/unix-time-to-iso8601-local .created))
           ;; `let-alist' does not work with `() syntax
           ,@(unless (equal "" .topic.value) (list (cons 'topic .topic.value)))
           ,@(unless (equal "" .purpose.value) (list (cons 'purpose .purpose.value))))))
      ;; Insert events / messages
      (seq-doseq (event (my/get-channel-events channel))
        (my/insert-event event)))))
}

The logic is roughly:

◊ul{
◊li{For each channel, insert its metadata and its messages.}
◊li{For each message, insert its metadata and its text, except if it's a list of files, in which case insert all the filenames.}
◊li{While inserting the text, replace user and channel mentions (that are exported as IDs) with their names, and format links as Org mode.}
}

◊subheading{Other comments}

◊highlight['elisp]{
(defun my/insert-properties (alist)
  "Insert ALIST as an Org property drawer."
  (insert ":PROPERTIES:\n")
  (map-do
   (lambda (k v)
     (insert ":" (upcase (format "%s" k)) ":  " v "\n"))
   alist)
  (insert ":END:\n\n"))
}

Omitting items from an alist conditionally is easier than doing the same with a string, hence this little helper.

◊highlight['elisp]{
(defun my/insert-text (text)
  "Insert TEXT with necessary newlines added amongst other processing."
  (save-match-data
    (insert
     (with-temp-buffer
       (insert text "\n\n")
       (goto-char (point-min))
       (while (re-search-forward "<\\(.*?\\)>" nil t)
         (let ((matched (match-string 1)))
           (cond ((string-prefix-p "@" matched)
                  (replace-match
                   (format "=@%s=" (alist-get 'name (my/get-user
                                                     (substring matched 1))))
                   t t))
                 ((string-prefix-p "http" matched)
                  (replace-match (format "[[%s]]" matched) t t))
                 ((string-prefix-p "#" matched)
                  (replace-match
                   (format "=#%s=" (alist-get 'name (my/get-channel
                                                     ;; Channel IDs are 9 digits
                                                     ;; + 1 for the #
                                                     (substring matched 1 10))))
                   t t)))))
       (buffer-string)))))
}

Slack exports user and channel mentions as ◊code{<@◊i{user ID}>} and ◊code{<#◊i{channel ID}>}, so to make it more readable I extracted the names from their respective JSON files. Links are exported as ◊code{<http://example.com>}, which doesn't work well in Org, so I also replace that with the Org syntax.

It is easier to work with buffers in Emacs than with strings, which is why I did the processing in another temporary buffer.

◊highlight['elisp]{
(defun my/file->json (file)
  "Return contents of FILE read through `json-read'."
  (save-match-data
    (with-temp-buffer
      (insert-file-contents-literally file)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (json-read))))

(defun my/array-files->json (&rest files)
  "Like `my/file->json', except that top-level arrays are merged."
  (cl-reduce
   (lambda (json-a json-b)
     (cl-merge 'list json-a json-b
               (lambda (elem-a elem-b)
                 (< (string-to-number (alist-get 'ts elem-a))
                    (string-to-number (alist-get 'ts elem-b))))))
   (mapcar #'my/file->json files)))
}

◊code{json-read} changes match data, so it needs to be wrapped in a ◊code{save-match-data}. This caused me a few minutes of pain as I tried to figure out why my ◊code{(while (re-search-forward ...) (replace-match ...))} didn't work.

◊code{my/file->json} is pretty straight forward, it just runs ◊code{json-read} on a file. ◊code{my/array-files->json} is less so. It is used to merge two JSON arrays together: as messages of the same channel are stored as multiple arrays in multiple files, getting all messages of a channel requires merging them. We use ◊code{cl-merge} to do the actual merging (the inner lambda is the comparasion function that ◊code{cl-merge} requires for its magic), and ◊code{cl-reduce} to make the two-input ◊code{cl-merge} work on the whole list of arrays.

◊highlight['elisp]{
(defun my/get-channel-events (channel)
  "Get events for CHANNEL.

CHANNEL can be either a string for its name, or an alist, in
which case the `name' property is used."
  (let ((name (cond ((stringp channel)
                     channel)
                    ((json-alist-p channel)
                     (alist-get 'name channel))
                    (t (error "CHANNEL must be a string or a `json-alist-p'")))))
    (apply #'my/array-files->json
           (directory-files name t "json$"))))
}

The use of ◊code{my/array-files->json}. I called them "events" here, but I later realized that all of them have the type "message".

◊heading{Full code}

◊highlight['elisp]{
;; -*- lexical-binding: t; -*-

(require 'json)
(require 'map)
(require 'seq)
(require 's)

;; Where I extracted the downloaded archive. For a script that isn't
;; written have reusability in mind, setting this is more convenient
;; than having to `defvar' and pass a path around.
;;
;; By the way, the current directory in Emacs is default-directory,
;; the current frame is (selected-frame), the current window (pane) is
;; (selected-window), and the current buffer is (current-buffer).
;; English and Emacs are both weird.
(setq default-directory "/tmp/slack-export/")

;; Used to break up long lines when trying to insert raw JSON objects
;; to see the data.
(defun my/fill-string (string)
  "Like `fill-paragraph', but on a STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (fill-paragraph)
    (buffer-string)))

(defun my/array-files->json (&rest files)
  "Like `my/file->json', except that top-level arrays are merged."
  (cl-reduce
   (lambda (json-a json-b)
     (cl-merge 'list json-a json-b
               (lambda (elem-a elem-b)
                 (< (string-to-number (alist-get 'ts elem-a))
                    (string-to-number (alist-get 'ts elem-b))))))
   (mapcar #'my/file->json files)))

(defun my/file->json (file)
  "Return contents of FILE read through `json-read'."
  (save-match-data
    (with-temp-buffer
      (insert-file-contents-literally file)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (json-read))))

(defun my/unix-time-to-iso8601-local (unix-timestamp)
  "Convert UNIX-TIMESTAMP into a ISO 8601 timestamp in local time.

Does not take leap seconds into account."
  (format-time-string "%FT%T%z" (seconds-to-time unix-timestamp)))

(defun my/get-user (user-id)
  "Get user JSON object from USER-ID."
  (seq-find
   (lambda (item)
     (equal (alist-get 'id item) user-id))
   (my/file->json "users.json")))

(defun my/get-channel (channel-id)
  "Get channel JSON object from CHANNEL-ID."
  (seq-find
   (lambda (item)
     (equal (alist-get 'id item) channel-id))
   (my/file->json "channels.json")))

(defun my/get-channel-events (channel)
  "Get events for CHANNEL.

CHANNEL can be either a string for its name, or an alist, in
which case the `name' property is used."
  (let ((name (cond ((stringp channel)
                     channel)
                    ((json-alist-p channel)
                     (alist-get 'name channel))
                    (t (error "CHANNEL must be a string or a `json-alist-p'")))))
    (apply #'my/array-files->json
           (directory-files name t "json$"))))

(defun my/insert-properties (alist)
  "Insert ALIST as an Org property drawer."
  (insert ":PROPERTIES:\n")
  (map-do
   (lambda (k v)
     (insert ":" (upcase (format "%s" k)) ":  " v "\n"))
   alist)
  (insert ":END:\n\n"))

(defun my/insert-text (text)
  "Insert TEXT with necessary newlines added amongst other processing."
  (save-match-data
    (insert
     (with-temp-buffer
       (insert text "\n\n")
       (goto-char (point-min))
       (while (re-search-forward "<\\(.*?\\)>" nil t)
         (let ((matched (match-string 1)))
           (cond ((string-prefix-p "@" matched)
                  (replace-match
                   (format "=@%s=" (alist-get 'name (my/get-user
                                                     (substring matched 1))))
                   t t))
                 ((string-prefix-p "http" matched)
                  (replace-match (format "[[%s]]" matched) t t))
                 ((string-prefix-p "#" matched)
                  (replace-match
                   (format "=#%s=" (alist-get 'name (my/get-channel
                                                     ;; Channel IDs are 9 digits
                                                     ;; + 1 for the #
                                                     (substring matched 1 10))))
                   t t)))))
       (buffer-string)))))

(defun my/insert-event (event)
  "Insert EVENT as Org format, handling some types."
  (let-alist event
    (insert "** " (my/unix-time-to-iso8601-local (string-to-number .ts)) "\n")
    (cond (.files
           (my/insert-properties '((type . "files")))
           (seq-doseq (file .files)
             (insert "=" (map-elt file 'name) "=\n")))
          ((equal "channel_join" .subtype)
           (my/insert-properties '((type . "event-join")))
           (my/insert-text .text))
          ((equal "channel_leave" .subtype)
           (my/insert-properties '((type . "event-leave")))
           (my/insert-text .text))
          ((equal "channel_purpose" .subtype)
           (my/insert-properties '((type . "event-set-purpose")))
           (my/insert-text .text))
          ((equal "channel_topic" .subtype)
           (my/insert-properties '((type . "event-set-topic")))
           (my/insert-text .text))
          ((equal "channel_name" .subtype)
           (my/insert-properties '((type . "event-set-name")))
           (my/insert-text .text))
          (t
           (my/insert-properties '((type . "message")))
           (my/insert-text .text)))))

(with-temp-file "slack.org"
  (insert "#+COLUMNS: %ITEM %CREATED %TOPIC %PURPOSE\n\n")
  (let ((channels (append (my/file->json "channels.json") nil)))
    (seq-doseq (channel channels)
      ;; Insert channel information
      (let-alist channel
        (insert "* =" .name "=\n")
        (my/insert-properties
         `((created . ,(my/unix-time-to-iso8601-local .created))
           ,@(unless (equal "" .topic.value) (list (cons 'topic .topic.value)))
           ,@(unless (equal "" .purpose.value) (list (cons 'purpose .purpose.value))))))
      ;; Insert events / messages
      (seq-doseq (event (my/get-channel-events channel))
        (my/insert-event event)))))

;; Local Variables:
;; mode: lisp-interaction
;; End:
}
