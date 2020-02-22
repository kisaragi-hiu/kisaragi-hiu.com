#lang pollen
◊file->string["emacs.css"]

◊(require
  racket/string
  racket/file
  css-expr)
◊(provide (all-defined-out))
◊(define cjk-fallback
   ◊string-append{'Noto Sans CJK TC',
                  'Microsoft Jhenghei',
                  'Microsoft Yahei',
                  'Meiryo',
                  'Malgun Gothic'})
◊(define sans-serif
   ◊string-append{'Overpass',
                  ◊|cjk-fallback|,
                  'sans-serif'})
◊(define title-sans-serif
   ◊string-append{'Overpass',
                  ◊|cjk-fallback|,
                  'sans-serif'})
◊(define monospace
   ◊string-append{'Overpass Mono',
                  'Noto Sans Mono CJK',
                  ◊|cjk-fallback|,
                  'monospace'})
◊(define heading "h1,h2,h3,h4,h5,h6,h7")
◊(define text-secondary "#555")
◊(define content "#content,#siteheader,#sitefooter-content")

/*
* Page margins
*/

◊|content| {
    margin: 0 20%;
}

/*
* Site Header
*/

#siteheader {
    display: flex;
    justify-content: space-between;
    flex-wrap: wrap;
}
#logo {
    display: flex;
    align-items: center;
}
#logo h1 {
    margin-top: 0.25rem;
}
#siteheader nav {
    display: flex;
    justify-content: flex-end;
}
#siteheader nav a {
    color: #6c757d;
    margin: 0.5rem 0.8rem 0.5rem 0;
    font-size: 16pt;
}

/*
* Site footer
*/

#sitefooter {
    color: ◊|text-secondary|;
}

#sitefooter p {
    line-height: 1.4;
    margin: 0.5rem auto 0.5rem;
}

#footer-sep {
    margin-top: 2.5rem;
    margin-bottom: 2.5rem;
    text-align: center;
    font-weight: 700;
}

#sitefooter nav {
    display: flex;
    justify-content: flex-start;
}
#sitefooter nav a {
    color: #6c757d;
    margin: 0.5rem 1rem 0.5rem 0;
    font-size: 1.2rem;
}

/*
* Project
*/

.project p {
  margin-top: 0;
}

.project-title {
  font-weight: 600;
}

/*
* TOC
*/
#toc-title {
    font-size: 16pt;
}
.toc {
    display: flex;
    flex-direction: column;
}
.toc-h1, .toc-h2 {
    font-weight: 400;
    font-size: 16pt;
}
.toc-h2 { text-indent: 1rem; }
.toc-h3 { text-indent: 2rem; }
.toc-h4 { text-indent: 3rem; }
.toc-h5 { text-indent: 4rem; }
.toc-h6 { text-indent: 5rem; }

/*
* Misc.
*/

.badge {
    margin: auto 0.5rem;
}

.image p {
    margin-top: 0;
    margin-bottom: 0;
}
.image-caption {
    text-align: center;
    font-family: monospace;
    font-style: italic;
}

blockquote {
    border-left: 0.3rem solid #ccc;
    margin: 1rem 0;
    padding: 0 0.3rem;
    font-style: italic;
    background: #f3f3f3;
}

◊(css-expr->css
  (css-expr
   [|div#references|
    #:margin-top 3rem
    #:color |#555|
    [p #:margin-top 0 #:margin-bottom 0]]))

.ref-desc-id {
    font-family: ◊|monospace|;
}

a {
    text-decoration: none;
}

body {
    margin: 2rem 0 3rem;
    line-height: 1.6;
    font-family: ◊|sans-serif|;
    font-weight: 400;
    color: #444;
    text-rendering: optimizeLegibility;
}

.page-navigation {
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    font-size: 16pt;
}

.disabled {
    color: ◊|text-secondary|;
}

p {
    font-size: 16pt;
}

html, body {
    background-color: #fbfbfb;
}

#logo img {
    max-height: 2rem;
    margin-right: 0.4rem;
}

a {
    color: #a868e8;
    -webkit-transition: all 0.4s ease;
    -moz-transition: all 0.4s ease;
    -o-transition: all 0.4s ease;
    -ms-transition: all 0.4s ease;
    transition: all 0.4s ease;
}

a:hover {
    color: #d0a3ff;
    text-decoration: none;
}

.text-primary {
    color: #333 !important;
}

a.text-primary:hover, a.text-primary:focus {
    color: #d0a3ff !important;
}

.index-item {
    margin-top: 0.5rem;
    margin-bottom: 0.5rem;
}

.index-item .date,
.index-item .category,
.index-item .tags {
    font-family: ◊|monospace|;
    display: block;
    color: ◊|text-secondary|;
    margin-bottom: 0;
    margin-top: 0;
    font-size: 1rem;
}

.highlight {
    font-size: 16pt;
    overflow-x: auto;
    font-family: ◊|monospace|;
    color: black;
}

◊; "+" h1,h2 h2,h3 => h1+h2,h2+h2,h1+h3,h2+h3
◊(css-op-all "+" heading heading) {
    margin-top: 1rem;
}

◊|heading| {
    font-size: 20pt;
    line-height: 1.2;
    margin-top: 2rem;
    margin-bottom: 0.5rem;
    font-weight: 300;
    letter-spacing: normal;
}

h1 {
    font-weight: 600;
    color: #444;
}

h2 { font-weight: 400; }

.pager {
    display: flex;
    justify-content: space-between;
    padding: 1em 0 1em 0;
}

.pager-prev::before { content: "←"; }
.pager-next::after  { content: "→"; }
.pager a {
    font-weight: 700;
}

.linenodiv pre {
    color: #aaa;
}

.image-link {
    -webkit-box-shadow: 0px 0px 10px 0px rgba(50, 50, 50, 0.68);
    -moz-box-shadow:    0px 0px 10px 0px rgba(50, 50, 50, 0.68);
    box-shadow:         0px 0px 10px 0px rgba(50, 50, 50, 0.68);
}

.ytembed-default {
    padding-bottom: 50%;
    position: relative;
    overflow: hidden;
}

.highlight .linenos {
    padding-right: 0.3rem;
}

@media (max-width: 1200px) {
    ◊|content| {
        margin: 0 5%;
    }
}
