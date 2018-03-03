#lang pollen
    Title: Hide decorations for maximized windows in Openbox, and using Pollen to preprocess the configuration
    Date: 2018-01-31T19:52:07
    Tags: language:en, category:Linux Configuration, Programming, Openbox, Pollen, Configuration

I switched from KWin to Openbox for my window manager in January, after thinking about getting off KWin for a while.

KWin is really powerful, but Plasma's configuration architecture prevents me from being able to keep it cleanly in version control. I tried out i3, but I can't (easily) use it in Plasma, which I do want to stay on. After reading a few articles on Openbox, and recalling my experience with its configuration around 2014, I decided to give it a try.

<blockquote class="twitter-tweet" data-lang="ja"><p lang="en" dir="ltr">Openbox is looking interesting now… Could be so much simpler than my current setup.<a href="https://t.co/eFR1laRr7C">https://t.co/eFR1laRr7C</a></p>&mdash; 如月.飛羽 (@flyin1501) <a href="https://twitter.com/flyin1501/status/956189883877416960?ref_src=twsrc%5Etfw">2018年1月24日</a></blockquote>

openbox rc.xml.pp:
```xml
  <keybind key="W-r">
    <action name="Execute"><command>raco pollen render ~/.config/openbox/</command></action>
    <action name="Execute"><command>openbox --reconfigure</command></action>
    ◊action/notify["Openbox --reconfigure complete" #:icon "view-refresh"]
  </keybind>
```

openbox/pollen.rkt:
```racket
(define/contract (action/notify summary [body " "] #:icon [icon #f])
  (->* (string?) (string? #:icon (or/c #f string?)) string?)
  (define command (string-join `("notify-send"
                                 ,(string-append "\""summary"\"")
                                 ,(string-append "\""body"\"")
                                 ,(if icon
                                      (string-append "--icon " "\""icon"\"")
                                      ""))))
  (xexpr->string
   `(action ([name "Execute"])
            (command ,command))))
```

Manually render rc.xml.pp once, now simply running Super-r will re-render the configuration and reconfigure openbox.

Primary use:
```xml
  <keybind key="W-Up W-k">
    ◊action/toggle-maximize-and-decorations[]
  </keybind>
```

```racket
(define (action/undecorate-and-maximize)
  (xexpr->string* '(action ([name "Undecorate"]))
                  '(action ([name "Maximize"]))))
(define (action/decorate-and-unmaximize)
  (xexpr->string* '(action ([name "Decorate"]))
                  '(action ([name "Unmaximize"]))))
(define (action/toggle-maximize-and-decorations)
  (xexpr->string `(action ([name "If"])
                          (maximized "yes")
                          (then ,(action/decorate-and-unmaximize))
                          (else ,(action/undecorate-and-maximize)))))
```
