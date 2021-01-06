#lang pollen
◊define-meta[title]{Locales, Org timestamps, and format-time-string}
◊define-meta[date]{2019-10-09T22:39:21+0900}
◊define-meta[tags ("Emacs" "Troubleshooting")]
◊define-meta[language]{en}
◊define-meta[toc #t]

◊heading{The problem}

Org mode's timestamps include a day-of-week part (like this: ◊code{<2019-10-09 Wed>}). That day-of-week is localized.

When ◊code{org-todo} is rescheduling a repeating task and encounters a timestamp with a day-of-week in a different locale (eg. ◊code{<2019-10-09 水 ◊link["https://orgmode.org/manual/Repeated-tasks.html"]{.+1d}>}), it updates the format but does not update the time described by the timestamp. When this happens, I have to run ◊code{org-todo} again, potentially creating a duplicate entry in the heading's logbook. This is normally not an issue, unless you edit the same Org file from two machines in different locales — exactly why I'm bitten by this quirk.

I use Emacs in ◊link["https://termux.com/"]{Termux} on my phone, which does not offer locales other than English (not sure if it's an English locale or POSIX). I use Emacs in server mode on my PC (set to the Japanese locale because I'm used to it), which for some reason does not honor ◊code{◊link["https://www.gnu.org/software/emacs/manual/html_node/elisp/Locales.html"]{system-time-locale}} and only uses the system-wide locale. Now I have two machines accessing the same Org file, stuck in two different locales.

There are several issues at display. It'd be nice if Termux, or Android, offers more locales. ◊code{org-todo} should update both the format ◊em{and} the time / date of the timestamp. Emacs should honor ◊code{system-time-locale} even when run in server mode.

All of the above is really complicated. For now, I just want a workaround to the problem.

◊subheading{Setting ◊code{system-time-locale}}

Before I found out this only works when I'm not using the Emacs daemon, I used this in my init file:

◊highlight['elisp]{
(setq system-time-locale "en")
(setenv "LANG" "en")
}

which normally makes ◊code{format-time-string} use English day-of-week names, and makes sure any processes started from this Emacs also do that. This did not work: ◊code{(format-time-string "%a")} still returns "水" on a Wednesday on my PC.

◊subheading{Use English on my PC}

Of course this workaround can always be done, but I don't want to change the rest of my system just to work around one issue in Emacs.

◊subheading{Setting ◊envvar{LANG} or ◊envvar{LC_TIME} for the server}

Another workaround is to set ◊envvar{LANG} so that Emacs server thinks the system is in the English locale.

As I use Emacs 26's new systemd unit, I have to copy the user unit to my home directory and use it to overwrite the unit in ◊path{/usr/share}.

◊path{◊envvar{HOME}/.config/systemd/user/emacs.service}:
◊highlight['ini]{
[Unit]
Description=Emacs text editor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=simple
ExecStart=/usr/bin/emacs --fg-daemon
ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
# added by me
Environment=LC_TIME=en
Restart=on-failure

[Install]
WantedBy=default.target
}

This also does not work. Something with the English locale, or the way I set it, makes it so that ◊link["https://en.wikipedia.org/wiki/Fcitx"]{Fcitx} doesn't work in Emacs anymore. I have to do something else.

◊heading{Advising ◊code{format-time-string}}

Eventually, I decided the best way to do this is probably to advise ◊code{format-time-string}. While the advice will not affect calls to it from C code, since Org is all Emacs Lisp, this should make sure Org timestamps are all in English.

Luckily, calendar.el provides ◊code{calendar-day-name} that returns day name for any date, so we simply have to convert an existing (or current) time value to Calendar's format (month, day, year).

◊highlight['elisp]{
(defun kisaragi/english-dow (&optional time zone abbreviated)
  "Return ABBREVIATED name of the day of week at TIME and ZONE.

If TIME or ZONE is nil, use `current-time' or `current-time-zone'."
  (unless time (setq time (current-time)))
  (unless zone (setq zone (current-time-zone)))
  (calendar-day-name
   (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_)
                (decode-time time zone)))
     (list m d y))
   abbreviated))
}

Then we need to parse the format, replace the ◊code{%a} and ◊code{%A} format strings.

◊highlight['elisp]{
(defun kisaragi/advice-format-time-string (func format &optional time zone)
  "Pass FORMAT, TIME, and ZONE to FUNC.

Replace \"%A\" in FORMAT with English day of week of today,
\"%a\" with the abbreviated version."
  (let* ((format (replace-regexp-in-string "%a" (kisaragi/english-dow time zone t)
                                           format))
         (format (replace-regexp-in-string "%A" (kisaragi/english-dow time zone nil)
                                           format)))
    (funcall func format time zone)))
}

Then add the advice:

◊highlight['elisp]{
(advice-add 'format-time-string :around #'kisaragi/advice-format-time-string)
}

Now ◊code{(format-time-string "%a")} always returns the abbreviated English day-of-week, regardless of system locale, and I don’t have to occasionally run ◊code{org-todo} twice anymore. Hopefully.

◊link["https://gitlab.com/kisaragi-hiu/.emacs.d/blob/971d47f0133b452aaf4c5d08e463430a9c0ffc47/.emacs.d/kisaragi/format-time-string-patch.el"]{File in my .emacs.d}.
