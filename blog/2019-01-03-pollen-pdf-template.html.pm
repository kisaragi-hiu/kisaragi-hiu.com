#lang pollen
◊define-meta[title]{A Pollen PDF template based on HTML, instead of LaTeX}
◊define-meta[date]{2019-01-03T03:50:00}
◊define-meta[category]{Programming}
◊define-meta[language]{en}
◊define-meta[toc #t]

<<<<<<< HEAD
◊edit["2019-03-24"]{
I forgot to mention that Chrom/ium adds a header and footer when printing to PDF from the command line. To work around this, set the top/bottom margin to 0, eg.:

◊highlight['css]{
@media print {
    body {
        margin: 0 2rem 0;
    }
}
}}

Pollen's manual has a ◊link["http://docs.racket-lang.org/pollen/fourth-tutorial.html#(part._.Adding_support_for_.P.D.F_output)"]{tutorial} on how to make ◊link["http://docs.racket-lang.org/pollen/second-tutorial.html#%28part._tutorial-2._.Templates%29"]{templates} for PDF. It first builds a LaTeX source file, then renders that with ◊command{pdflatex}, then returns the bytes from the PDF, deleting the temporary files. The problem with this in my use case is that I don't use LaTeX, and would like a way to just turn the HTML version of my pages into PDF. So far I had always just manually rendered my pages in Firefox (my primary browser) or Chrome (offers a print preview), which I'd like to automate.
=======
Pollen's manual has a ◊link["http://docs.racket-lang.org/pollen/fourth-tutorial.html#(part._.Adding_support_for_.P.D.F_output)"]{tutorial} on how to make ◊link["http://docs.racket-lang.org/pollen/second-tutorial.html#%28part._tutorial-2._.Templates%29"]{templates} for PDF. It first builds a LaTeX source file, then renders that with ◊command{pdflatex}, then returns the bytes from the PDF, deleting the temporary files. The problem with this in my use case is that I don't use LaTeX, and would like a way to turn the HTML version of my pages into PDF. So far I had always manually rendered my pages in Firefox (my primary browser) or Chrome (offers a print preview), which I'd like to automate.
>>>>>>> origin/source

First was finding a programmable interface to render an HTML to PDF. I knew Chrom/ium has a ◊code{--print-to-pdf} option, but initially I thought it's going to have unavoidable headers and footers. So I started working with ◊link["https://wkhtmltopdf.org/"]{◊command{wkhtmltopdf}}.

◊heading{First version with ◊command{wkhtmltopdf}}

First we check for ◊command{wkhtmltopdf}'s existance.

◊highlight['racket]{
◊"◊"(unless (find-executable-path "wkhtmltopdf")
   (error 'pdf-render "wkhtmltopdf missing"))
}

Then we assemble the pagenode to render, then send it to ◊code{render-pagenodes} to render. Using ◊code{render-pagenodes} allows me to specify that I want to get HTML output.

◊highlight['racket]{
◊"◊"(define html-pagenode
   (string->symbol
    (string-replace (symbol->string here)
                    #rx"\\.pdf$" ".html")))
◊"◊"(render-pagenodes `(root ,html-pagenode))
}

Lastly, we send the HTML to ◊command{wkhtmltopdf}, telling it to output to stdout, and capture it with ◊code{with-output-to-bytes}. (◊code{(thunk ...)} is shorthand for ◊code{(lambda () ...)}.)

◊highlight['racket]{
◊"◊"(with-output-to-bytes (thunk (system (format "wkhtmltopdf ~a -" html-pagenode))))
}

The entire thing: ◊gist{kisaragi-hiu/47578b77677bf659982819125cc34df4}

◊subheading{◊command{wkhtmltopdf} troubles}

After getting the rendered PDF and printing them out, I found that the output from ◊command{wkhtmltopdf} differed too much from what I get from either Firefox or Chrome. Some of the CSS I have didn't seem to work as well. I then realized Chrome's ◊code{--print-to-pdf} option doesn't give me the headers and footers with the CSS I have, so I decided using Chrome was less trouble.

One problem with this is that Chrome has to output to a file, so I couldn't write to stdout to avoid using a temporary file, like I did with ◊command{wkhtmltopdf}.

◊heading{Second version with Chrome}

First we try to find the command for Chrome. The ◊code{error} only runs when all above fails.

◊highlight['racket]{
◊"◊"(define chrome-executable
   (or (find-executable-path "chromium")
       (find-executable-path "google-chrome-stable")
       (find-executable-path "google-chrome-unstable")
       (find-executable-path "google-chrome")
       (find-executable-path "chrome")
       (error 'pdf-render "chrome missing")))
}

Let Racket handle making the temporary file for us. (Pardon my naming sense…)

◊highlight['racket]{
◊"◊"(define temp-output (make-temporary-file))
}

This is the same as before, except this time we call ◊code{system} here and let it do its thing, suppressing its output with ◊code{void}.

◊highlight['racket]{
◊"◊"(define html-pagenode
   (string->symbol
    (string-replace (symbol->string here)
                    #rx"\\.pdf$" ".html")))
◊"◊"(void
  (render-pagenodes `(root ,html-pagenode))
  (system (format "~a --headless --disable-gpu --print-to-pdf=~a ~a"
                  chrome-executable
                  temp-output
                  html-pagenode)))
}

Returning the bytes has to be done last for some reason, so we capture the bytes in ◊code{output}, delete the temporary file, then return ◊code{output}.

◊highlight['racket]{
◊"◊"(define output (file->bytes temp-output))
◊"◊"(delete-file temp-output)
◊"◊"output
}

Full version: ◊gist{kisaragi-hiu/6062b66694a612fd6981f0afc4515b1e}
