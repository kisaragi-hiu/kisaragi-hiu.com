#lang pollen
◊define-meta[title]{◊span{Lookup Cangjie encoding in Emacs with ◊code{cangjie.el}}}
◊define-meta[date]{2019-01-21T19:43:47+09:00}
◊define-meta[tags ("Emacs" "Cangjie")]
◊define-meta[category]{Projects}
◊define-meta[language]{en}
◊define-meta[toc #t]

In August 2018, I wanted to start learning ◊link["https://en.wikipedia.org/wiki/Cangjie_input_method"]{Cangjie input method}, but I didn't know of an easy way to look up the encoding for particular characters. Up to this point I had been using Chinese Wiktionary, which contains the encoding in the pages about individual characters.

◊heading{◊code{curl Wiktionary | grep "仓颉"}}

Getting that data from Wiktionary is simple: ◊code{curl "https://zh.wiktionary.org/wiki/<character>" | grep "仓颉"}. I initially had this saved as a shell script, but as I use Emacs a lot, I wanted to have the function available in Emacs. The ◊code{cangjie} function receives the character and passes it to the pipeline, written in Emacs Lisp with ◊link["https://github.com/magnars/dash.el"]{◊code{dash.el}}’s threading macro.

This approach has a problem. Even though most Wiktionary pages do contain the information I need, that’s not guaranteed in any way. Plus, reaching for a server everytime I want to look up a character means it’s going to be slow. So I wanted to look for a Cangjie dictionary, and the best option that came to mind was to use the dictionary from ◊link["https://rime.im/"]{RIME}.

◊heading{Using RIME’s Cangjie dictionary}

RIME is an ◊link["https://en.wikipedia.org/wiki/Input_method"]{IME} for Chinese languages. It has built-in support for Cangjie (all Han characters), Pinyin, Zhuyin (Mandarin), Jyutping (Cantonese), among other input methods. For my use case, it has a Cangjie dictionary ◊github["rime/rime-cangjie"]{available on GitHub} that uses a much more stable format than Wiktionary entries.

The first version I committed to the ◊github["kisaragi-hiu/cangjie.el"]{cangjie.el repository} already had both of these approaches; in this version, which approach to use is controlled by whether a valid RIME dictionary exists or not.

RIME dictionaries (◊link["https://github.com/rime/home/wiki/RimeWithSchemata#%E7%A2%BC%E8%A1%A8%E8%88%87%E8%A9%9E%E5%85%B8"]{official documentation}) are ◊code{\t}-seperated values with a YAML header in front, using # as the comment character. To get the Cangjie encoding for a character from the dictionary, we grab the lines matching the character, remove any matches starting with a #, then select the second element delimited by ◊code{\t}. This encoding is written in the alphabetical representation of the Cangjie code (like ◊code{jnd}), so we convert that to the Han character representation (like ◊code{十弓木}), implemented with ◊code{cangjie--abc-to-han}.

◊highlight['elisp]{
(defun cangjie (han)
  "Retrieve Cangjie code for the HAN character."
  (cond ((cangjie--valid-rime-dict? cangjie-source)
         ;; take cangjie encoding from RIME dictionary
         (->> (cangjie--grep-line cangjie-source han)
              (--filter (not (s-prefix? "#" it)))
              (s-join "")
              (s-split "\t")
              second
              cangjie--abc-to-han))
        ((eq cangjie-source 'wiktionary)
         ;; Try to extract encoding from grep'd wiktionary text
         (->> (shell-command-to-string
               (concat "curl --silent https://zh.wiktionary.org/wiki/" han
                       " | grep 仓颉"))
              (s-replace-regexp "^.*：" "")
              s-trim
              (s-replace-regexp "<.*>$" "")
              cangjie--abc-to-han))
        (t
         ;; Fallback
         (shell-command-to-string
          (concat "curl --silent https://zh.wiktionary.org/wiki/" han
                  " | grep 仓颉")))))
}

After this, I added code to automatically download the RIME dictionary, a customize option to control whether to use Wiktionary or RIME, and rewrote some code for taste. This is when I considered the package essentially complete.

◊heading{Submitting to MELPA}

In October, I came across ◊link["https://spin.atomicobject.com/2016/05/27/write-emacs-package/"]{Take Your Emacs to the Next Level by Writing Custom Packages}, where the author writes about their experience writing their first package. In particular, there’s a section about how they submitted their package onto MELPA, and it made me consider submitting to MELPA as well.

I’ve set up Flycheck a long time ago, and have generally always agreed with the warnings it gave me about docstrings and code style. After going through ◊link["https://github.com/melpa/melpa/blob/2c70b4f5d62fcd1df998af325342aa082c7e939d/CONTRIBUTING.org"]{MELPA’s official guide} on submission, I opened a pull request and got accepted. It was a pleasant experience.
