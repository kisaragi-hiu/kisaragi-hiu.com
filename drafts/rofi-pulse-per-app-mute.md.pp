#lang pollen
    Title: Mute individual applications with Rofi
    Date: ◊date◊
    Tags: language:◊lang◊, category:◊cat◊, ◊tags◊

In KDE, the audio volume widget contains a mixer that allows per-application muting and volume adjustments. However, I want to be able to do the same, or at least be able to mute each app, with just the keyboard.

◊image["/blog/images/kde-volume-mixer.jpg" "The KDE volume mixer."]

I used Rofi for this, though any interactive filter would work.

◊image["/blog/images/rofi-per-app-mute-thingy.png" "The final result."]

To get the information I need from the command line, I used `pactl`:

```bash
$ pactl list sink-inputs
Sink Input #10
    Driver: protocol-native.c
    Owner Module: 14
    ...
        Properties:
            media.name = "audio stream #1"
            application.name = "osu!"
        ...
            application.process.binary = "wine-preloader"
        ...

Sink Input #16
    Driver: protocol-native.c
    Owner Module: 14
    ...
    Properties:
        media.name = "AudioStream"
        application.name = "AudioIPC Server"
        ...
        application.process.binary = "firefox"
        ...
```

Then I need to format this so it's suitable for giving to Rofi. Instead of trying to `sed` my way through it, I ended up writing this in Emacs, as I have the Emacs daemon set up. (The function name is slightly inaccurate, but well.)

```elisp
(defun get-pulse-sink-input-ids ()
  "Get pulse sink inputs, and format them into a format suitable for outputting to rofi."
  (--> (shell-command-to-string
        "pactl list sink-inputs \\
         | grep -e 'Sink Input #' -e 'binary =' -e 'application.name =' \\
         | sed 's/\t*//g'")
       (s-split "\n" it t)
       (-partition 3 it)  ; '(a b c d e f) => '((a b c) (d e f))
       (-map (lambda (l) (-sort #'string-greaterp l)) it)
       ;; at this point:
       ;; (("application.process.binary = \"wine-preloader\""
       ;;   "application.name = \"osu!\""
       ;;   "Sink Input #17"))
       (-map (lambda (l) (concat (s-replace "Sink Input #" "" (third l))
                                 ": "
                                 (--> (first l)
                                      (s-replace "application.process.binary = " "" it)
                                      (string-trim it "\"" "\""))
                                 " ("
                                 (--> (second l)
                                      (s-replace "application.name = " "" it)
                                      (string-trim it "\"" "\""))
                                 ")"))
             it)
       ;; now: ("17: wine-preloader (osu!)")
       (s-join "\n" it)))
```

Now from shell, I can use `emacsclient --eval` to run the function and get the value I need. It's in the printed form of the value though, so for now I'm just 

```bash
$ emacsclient --eval '(get-pulse-sink-input-ids)'
"10: wine-preloader (osu!)\n17: firefox (AudioIPC Server)"
```
