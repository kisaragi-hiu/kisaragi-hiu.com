#lang pollen
◊define-meta[title]{◊span{Three versions of ◊code{random-string}}}
◊define-meta[date]{2018-10-08T20:52:35+0900}
◊define-meta[language]{en}
◊define-meta[category]{Experiments}
◊define-meta[toc #t]

After I started using a password manager to generate my passwords, sometimes I want to do the same in the shell — not necessarily for passwords, but just a random alphanumeric string of a specified length can be useful sometimes.

The command would take just one argument as the length for the output string.

I wrote the ◊gitlab["kisaragi-hiu/dotfiles/commit/c5946b85625d0f10d93b0350f9a34a355293ea6d"]{first version} in Racket, then rewrote it in Common Lisp because Racket's startup speed makes it quite unsuitable for shell commands. I eventually rewrote it in Picolisp because it's available in Termux; it's quite a fascinating language.

◊heading{the Racket version}

◊gitlab["kisaragi-hiu/dotfiles/blob/ed9483a72adcc32ac8935a59f85b61b7e574240f/random-string.rkt"]{◊code{random-string.rkt}}

Define all the characters that could be used — it's just hardcoded alphanumeric characters because this is my only use case. Here if the environment variable ◊code{CHARSET} is set it would use that instead.

◊highlight['racket]{
(define charset (or (getenv "CHARSET")
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
}

The main logic is just making a list of specified length, then choosing a random item (with ◊code{select-random-item}  from the charset for each list item, converting the list into a string in the end.

◊highlight['racket]{
(define (select-random-item seq)
  (sequence-ref seq (random (sequence-length seq))))

(define (random-string [len 16])
  (list->string
    (map (λ (x) (select-random-item charset))
         (make-list len #f))))
}

The last section is the entry point; when there are arguments passed into the script, take the 0th argument and pass it to ◊code{random-string}

◊highlight['racket]{
(if (empty? (vector->list (current-command-line-arguments)))
  (displayln (random-string))
  (displayln (random-string (string->number (vector-ref (current-command-line-arguments) 0)))))
}

◊heading{Common Lisp rewrite}

◊gitlab["kisaragi-hiu/dotfiles/blob/ed9483a72adcc32ac8935a59f85b61b7e574240f/random-string.cl"]{◊code{random-string.cl}}

The logic is almost exactly the same as the Racket version, though here I didn't bother with looking for the ◊code{CHARSET} environment variable.

◊highlight['lisp]{
(defvar *charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
}

One thing that surprised me initially is the existence of "random states". Learning about little things like this is exactly why I like writing simple scripts like this.

◊highlight['lisp]{
(defun select-random-item (seq)
  (elt seq (random (length seq)
                   ;; create a newly randomly seeded random state
                   (make-random-state t))))
}

Looking at this now, I probably should've written the Racket version with ◊code{sequence-map} operating directly on a string.

◊highlight['lisp]{
(defun random-string (&optional (len 16))
  (map 'string
       (lambda (x) (select-random-item *charset*))
       (make-string len)))
}

Here I depend on CLISP by using ◊code{*args*}

◊highlight['lisp]{
(format t (if *args*
            (random-string (parse-integer (first *args*)))
            (random-string)))
}

◊heading{Writing the Picolisp version}

◊gitlab["kisaragi-hiu/dotfiles/blob/5e39e6c94b2c4fd3c595e10280ce8a38284bd149/random-string"]{◊code{random-string}}

I first saw Picolisp because it is currently (as of 2018-10) the only Lisp family language packaged in Termux.

In Picolisp, command line arguments can't be accessed directly in the program. The interpreter itself processes arguments as files to load or functions to invoke. This becomes a problem when the whole point of the script is to be a shell command.

There are two ways around this: one is to simply wrap the whole thing into a shell script, and handle arguments there. That seemed unstatisfying to me (I don't know why now that I've realized how hacky this next solution is), so I went with a hack to bind the first command line argument to a variable in the script.

◊code{pil} reads command line arguments starting with a "-" as a function to run; or, more specifically, as a form to evaluate after wrapping it in parentheses. For example, ◊code{-bye} runs ◊code{(bye)} immediately, exiting the program; ◊code{-argv dummy len} runs ◊code{(argv dummy len)}  which binds the first command line argument to ◊code{dummy} and second to ◊code{len}  but those arguments are still going to be loaded as files.

If ◊code{pil} sees an argument that's just a "-", it'll stop loading subsequent arguments as files. This would've been the solution: ◊code{pil -"argv dummy dummy len" random-string - 60} would bind ◊code{dummy} to "random-string", then to "-", then ◊code{len} to "60", after which it'll load my script where ◊code{len} is available. There's just one problem: I have to write this command in the shebang.

The shebang tells the kernel to pass this file to the specified program, like ◊code{#!/bin/bash}  It can also pass an argument to the program, for instance, ◊code{#!/usr/bin/pulseaudio -nF} in some PulseAudio config files. The problem is, it can only pass one argument: everything after the space in the shebang is passed to the program as $1 in shell notation. This means I can't pass another "-" argument to ◊code{pil}

In the end, my shebang looks like ◊code{#!/usr/bin/pil -argv dummy len}  bind the first argument (path to script) to ◊code{dummy}  second argument to ◊code{len}  ◊code{len} isn't actually going to be loaded because ◊code{(bye)} has been called before its loading starts.

◊highlight['lisp]{
#!/usr/bin/pil -argv dummy len
# a bit of a hack around Picolisp's loading mechanism
# random-string [length]
}

Here I have to seed the PRNG with current time before running ◊code{select-random-item}  because I couldn't find a way to get time more accurate than seconds. If I seed it inside ◊code{select-random-item}  it'd receive the same (fresh) seed and thus return the same character throughout the second.

◊highlight['lisp]{
(seed (+ (date) (time)))
(setq *charset* (chop "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

(de select-random-item (seq)
  (car (nth seq
            (rand 1 (length seq)))))
}

Picolisp doesn't have defaults for optional arguments, so I have to set it myself when the input is nil.

Another interesting thing about Picolisp is that it actually uses a list of form ◊code{((arg1 arg2 ...) body)} as functions. Personally I think this is quite elegant, and would like to see more non-functions that are applicable like this in other Lisps as well. Allowing lists to be applicable like functions shouldn't break anything… I think.

◊highlight['lisp]{
(de random-string (len)
  (if (not len) (setq len 16))
  (if (str? len) (setq len (format len)))
  (mapcar '(() (select-random-item *charset*))
          (range 1 len)))

(prinl (random-string len))
(bye)
}
