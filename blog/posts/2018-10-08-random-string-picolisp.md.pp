#lang pollen
    Title: Three versions of `random-string`
    Date: 2018-10-08T20:52:35
    Tags: language:en, Picolisp, Programming, Racket, Lisp

After I started using a password manager to generate my passwords, sometimes I want to do the same in the shell — not necessarily for passwords, but just a random alphanumeric string of a specified length can be useful sometimes.

The command would take just one argument as the length for the output string.

I wrote the ◊link["https://gitlab.com/kisaragi-hiu/dotfiles/commit/c5946b85625d0f10d93b0350f9a34a355293ea6d"]{first version} in Racket, then rewrote it in Common Lisp because Racket's startup speed makes it quite unsuitable for shell commands. I eventually rewrote it in Picolisp because it's available in Termux; it's quite a fascinating language.

## the Racket version

```racket
#!/usr/bin/env racket
#lang racket
```

Define all the characters that could be used — it's just hardcoded alphanumeric characters because this is my only use case. Here if the environment variable `CHARSET` is set it would use that instead.

```racket
(define charset (or (getenv "CHARSET")
                    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
```

The main logic is just making a list of specified length, then choosing a random item (with `select-random-item`) from the charset for each list item, converting the list into a string in the end.

```racket
(define (select-random-item seq)
  (sequence-ref seq (random (sequence-length seq))))

(define (random-string [len 16])
  (list->string
    (map (λ (x) (select-random-item charset))
         (make-list len #f))))
```

The last section is the entry point; when there are arguments passed into the script, take the 0th argument and pass it to `random-string`.

```racket
(if (empty? (vector->list (current-command-line-arguments)))
  (displayln (random-string))
  (displayln (random-string (string->number (vector-ref (current-command-line-arguments) 0)))))
```

## Common Lisp rewrite

```lisp
#!/usr/bin/env clisp
; random-string [length]
```

The logic is almost exactly the same as the Racket version, though here I didn't bother with looking for the `CHARSET` environment variable.

```lisp
(defvar *charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
```

One thing that surprised me initially is the existence of "random states". Learning about little things like this is exactly why I like writing simple scripts like this.

```lisp
(defun select-random-item (seq)
  (elt seq (random (length seq)
                   ;; create a newly randomly seeded random state
                   (make-random-state t))))
```

Looking at this now, I probably should've written the Racket version with `sequence-map` operating directly on a string.

```lisp
(defun random-string (&optional (len 16))
  (map 'string
       (lambda (x) (select-random-item *charset*))
       (make-string len)))
```

Here I depend on CLISP by using `*args*`.

```lisp
(format t (if *args*
            (random-string (parse-integer (first *args*)))
            (random-string)))
```

## Writing the Picolisp version

I first saw Picolisp because it is currently (as of 2018-10) the only Lisp family language packaged in Termux.

In Picolisp, command line arguments can't be accessed directly in the program. The interpreter itself processes arguments as files to load or functions to invoke. This becomes a problem when the whole point of the script is to be a shell command.

There are two ways around this: one is to simply wrap the whole thing into a shell script, and handle arguments there. That seemed unstatisfying to me (I don't know why now that I've realized how hacky this next solution is), so I went with a hack to bind the first command line argument to a variable in the script.

`pil` reads command line arguments starting with a "-" as a function to run; or, more specifically, as a form to evaluate after wrapping it in parentheses. For example, `-bye` runs `(bye)` immediately, exiting the program; `-argv dummy len` runs `(argv dummy len)`, which binds the first command line argument to `dummy` and second to `len`, but those arguments are still going to be loaded as files.

If `pil` sees an argument that's just a "-", it'll stop loading subsequent arguments as files. This would've been the solution: `pil -"argv dummy dummy len" random-string - 60` would bind `dummy` to "random-string", then to "-", then `len` to "60", after which it'll load my script where `len` is available. There's just one problem: I have to write this command in the shebang.

The shebang tells the kernel to pass this file to the specified program, like `#!/bin/bash`. It can also pass an argument to the program, for instance, `#!/usr/bin/pulseaudio -nF` in some PulseAudio config files. The problem is, it can only pass one argument: everything after the space in the shebang is passed to the program as $1 in shell notation. This means I can't pass another "-" argument to `pil`.

In the end, my shebang looks like `#!/usr/bin/pil -argv dummy len`: bind the first argument (path to script) to `dummy`, second argument to `len`; `len` isn't actually going to be loaded because `(bye)` has been called before its loading starts.

```picolisp
#!/usr/bin/pil -argv dummy len
# a bit of a hack around Picolisp's loading mechanism
# random-string [length]

# (time) is only down to seconds, can't just seed the PRNG on every call to select-random-item.
(seed (+ (date) (time)))
(setq *charset* (chop "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

(de select-random-item (seq)
  (car (nth seq
            (rand 1 (length seq)))))

(de random-string (len)
  (if (not len) (setq len 16))
  (if (str? len) (setq len (format len)))
  (mapcar '(() (select-random-item *charset*))
          (range 1 len)))

(prinl (random-string len))
(bye)
# vim: filetype=picolisp
```
