#lang pollen
◊define-meta[title]{QR Codes from Emacs}
◊define-meta[date]{2019-09-23T01:51:44+0900}
◊define-meta[category]{Emacs}
◊define-meta[language]{en}

◊heading{Why QR codes?}

https://blog.jpalardy.com/posts/qr-codes-on-the-command-line/
https://blog.jpalardy.com/posts/qr-codes-on-the-command-line/
https://blog.jpalardy.com/posts/qr-codes-on-the-command-line/

◊heading{From Emacs}

Install ◊link["https://fukuchi.org/works/qrencode/index.html.en"]{◊code{qrencode}} on your system:

◊highlight['bash]{
pacman -S qrencode
}

Generate a QR code by directly calling the shell command and dump it to an ASCII art:

◊highlight['elisp]{
(shell-command-to-string "qrencode 'hello world' -t UTF8 -o -")
}

You can now see the code in the ◊code{*Messages*} buffer.

◊image[]{qrencode output in *Messages*}

Of course, we should utilize Emacs's ability to display images; here's a Emacs command that prompts for the string, encodes it, and displays a nice PNG buffer.

◊video/gif-esque[]{kisaragi/qr-encode accepting input, displaying qr buffer}

◊highlight['elisp]{
(defun kisaragi/qr-encode (str &optional buf)
  "Encode STR as a QR code.

Return a new buffer or BUF with the code in it."
  (interactive "MString to encode: ")                    ; Open a prompt; pass input as str
  (let ((buffer (get-buffer-create (or buf "*QR Code*"))))
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
