#lang pollen
◊define-meta[title]{QR Codes from Emacs}
◊define-meta[date]{2019-09-24T22:45:48+0900}
◊define-meta[category]{Emacs}
◊define-meta[language]{en}
◊;define-meta[toc #t]

This article was inspired by ◊link["https://blog.jpalardy.com/posts/qr-codes-on-the-command-line/"]{QR Codes on the Command-Line}.

QR codes are nice: if you want to send e.g. a URL or a short string to your phone, just turn it into a QR code and scan it on the phone, no internet required.

Install ◊link["https://fukuchi.org/works/qrencode/index.html.en"]{◊code{qrencode}} on your system:

◊highlight['bash]{
pacman -S qrencode
}

Now a QR code can be generated in Emacs by directly calling the shell command, then dumping it to an ASCII art in the ◊code{*Messages*} buffer:

◊highlight['elisp]{
(shell-command-to-string "qrencode 'hello world' -t UTF8 -o -")
}

◊image["/static/emacs-qrencode-as-ascii.png" #:max-height "25rem"]{qrencode output in *Messages*}

Of course, we should utilize Emacs's ability to display images. Here's a Emacs command that prompts for the string, encodes it with ◊code{qrencode}, and displays it in a nice PNG buffer.

◊subheading[#:id "k-q-gui-only"]{◊code{kisaragi/qr-encode}}

◊highlight['elisp]{
(defun kisaragi/qr-encode (str &optional buf)
  "Encode STR as a QR code.

Return a new buffer or BUF with the code in it."
  (interactive "MString to encode: ")                    ; Open a prompt; pass input as str
  (let ((buffer (get-buffer-create (or buf "*QR Code*")))
        (inhibit-read-only t))                           ; make sure we can write the output
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))           ; Clear the buffer
    (make-process                                        ; Start a process
     :name "qrencode" :buffer buffer                     ; that writes stdout to `buffer'
     :command `("qrencode" ,str "-t" "PNG" "-o" "-")     ; "-o -" sends output to stdout
     :coding 'no-conversion                              ; Don't encode stdout as string
     :sentinel (lambda (_process change)
                 (when (string= change "finished\n")     ; If the process successfully exits,
                   (with-current-buffer buffer           ; then go to the buffer,
                     (image-mode)                        ; display its contents as PNG,
                     (image-transform-fit-to-height))))) ; and resize it so it's not tiny
    (when (called-interactively-p 'interactive)
      (display-buffer buffer))
    buffer))
}

◊video/gif-esque["/static/emacs-kisaragi-qr-encode.mp4" #:max-height "40rem"]{kisaragi/qr-encode accepting input, displaying qr buffer}

◊heading{In the Terminal}

For ◊link["https://blog.aaronbieber.com/2016/12/29/don-t-use-terminal-emacs.html"]{as nice as GUI Emacs is}, sometimes I just have to use Emacs in a terminal — specifically, on Android, under ◊link["https://termux.com/"]{Termux}. This version is what I have in ◊gitlab["kisaragi-hiu/.emacs.d"]{my Emacs configuration} right now (2019-09-24), which uses ASCII art output if it is run in a terminal.

◊subheading[#:id "k-q-with-non-gui"]{◊code{kisaragi/qr-encode} with non-GUI support}

◊highlight['elisp]{
(defun kisaragi/qr-encode (str &optional buf)
  "Encode STR as a QR code.

Return a new buffer or BUF with the code in it."
  (interactive "MString to encode: ")
  (let ((buffer (get-buffer-create (or buf "*QR Code*")))
        (format (if (display-graphic-p) "PNG" "UTF8"))        ; use PNG in a graphical frame
        (inhibit-read-only t))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (make-process
     :name "qrencode" :buffer buffer
     :command `("qrencode" ,str "-t" ,format "-o" "-")
     :coding 'no-conversion
     ;; seems only the filter function is able to move point to top
     :filter (lambda (process string)
               (with-current-buffer (process-buffer process)  ; Make sure we're looking at the
                 (insert string)                              ; top of the output buffer
                 (goto-char (point-min))
                 (set-marker (process-mark process) (point))))
     :sentinel (lambda (_process change)
                 (when (string= change "finished\n")
                   (with-current-buffer buffer
                     (cond ((string= format "PNG")
                            (image-mode)
                            (image-transform-fit-to-height))
                           (t                                 ; decode the output as we
                            (text-mode)                       ; suppressed it at line 14
                            (decode-coding-region (point-min) (point-max) 'utf-8)))))))
    (when (called-interactively-p 'interactive)
      (display-buffer buffer))
    buffer))
}
