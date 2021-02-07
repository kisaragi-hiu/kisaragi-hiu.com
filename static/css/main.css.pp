#lang pollen
◊file->string["emacs.css"]
/* ◊;file->string["tabbed.css"] */

◊(require
  racket/string
  racket/file
  "../../rkt/css.rkt")

◊(define cjk-fallback
  `("Noto Sans CJK TC"
    "Microsoft Jhenghei"
    "Microsoft Yahei"
    "Meiryo"
    "Malgun Gothic"))
◊(define sans-serif
  `("Inter" ,@cjk-fallback sans-serif))
◊(define title-sans-serif sans-serif)
◊(define monospace
  `("Overpass Mono" "Noto Sans Mono CJK" ,@cjk-fallback monospace))

◊(define heading "h1,h2,h3,h4,h5,h6,h7")
◊(define color-primary "#0d0d0d")
◊(define color-secondary "#333")
◊(define color-accent "#4d2b82") /* Light purple */
◊(define color-accent-strong "#cdadff") /* Dark purple */
◊(define color-special "#cdecff") /* Light blue */
◊(define color-special-strong "#246084") /* Dark blue */
/* If you decide you want a pink, use #ffade0. */

◊(define font-size-main "14pt")
◊(define font-size-xl "24pt")
◊(define font-size-big "16pt")

/* Base */

li p {
    margin: 0 auto;
}

body, #siteheader nav a, .size-normal, .toc *, .index-item .date, .index-item .tags, .page-navigation, .highlight {
  font-size: ◊|font-size-main|;
}

#siteheader,
.list {
  flex-wrap: wrap;
}

.tag {
  background-color: ◊|color-special|;
  color: ◊|color-special-strong|;
  padding: 0 0.25rem;
  border-radius: 0.25rem;
}

.tag::before {
  content: "#";
}

body {
  margin: 2em 20%;
  line-height: 1.6;
  font-family: ◊&[sans-serif];
  font-weight: 400;
  color: ◊|color-primary|;
  text-rendering: optimizeLegibility;
}

video,
figure,
img {
  max-width: 100%;
}

/*
* Utilities */

.maxh-40rem {
  max-height: 40rem;
}

.maxh-25rem {
  max-height: 25rem;
}

.disabled {
  color: ◊|color-secondary|;
}

/*
* Site Header */

#siteheader {
  display: flex;
  justify-content: space-between;
  align-items: center;
}
#logo h1 {
  align-items: center;
  display: flex;
}
#logo img {
  max-height: 2rem;
  margin-right: 0.4rem;
}
#siteheader nav {
  display: flex;
  justify-content: flex-end;
}
#siteheader nav a {
  color: ◊|color-secondary|;
  margin: 0.5rem 0.8rem 0.5rem 0;
}

/*
* Site footer */

#sitefooter {
  color: ◊|color-secondary|;
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
  justify-content: center;
}

#sitefooter nav a {
  color: ◊|color-secondary|;
  margin: 0.5rem 1rem 0.5rem 0;
}

/*
* Project */

.project p {
  margin-top: 0;
}

.project-title {
  font-weight: 600;
}

/*
* Builtin tags */

/* css-op-all: "+" h1,h2 h2,h3 => h1+h2,h2+h2,h1+h3,h2+h3 */
◊(css-op-all "+" heading heading) {
  margin-top: 1rem;
}

.xl {
  font-size: ◊|font-size-xl|;
}

◊|heading| {
  font-size: ◊|font-size-big|;
  line-height: 1.2;
  margin-top: 2rem;
  margin-bottom: 0.5rem;
  font-weight: 300;
  letter-spacing: normal;
}

h1 {
  font-weight: 700;
  color: ◊|color-primary|;
}

h2 {
  font-weight: 600;
}

h3 {
  margin-bottom: 0;
}

blockquote {
  border-left: 0.3rem solid #ccc;
  margin: 1rem 0;
  padding: 0 0.3rem;
  font-style: italic;
  background: #f3f3f3;
}

a {
  color: ◊|color-accent|;
  opacity: 0.9;
  transition: opacity 100ms ease;
  text-decoration: none;
}

a:hover {
  opacity: 1;
  text-decoration: none;
}

/*
* Reference */

div#references {
  margin-top: 3rem;
}

div#references p {
  margin-top: 0;
  margin-bottom: 0;
}

.div#references {
  color: #555;
}

.ref-desc-id {
  font-family: ◊&[monospace];
}

/*
* Index items */

.index {
  margin-top: 2rem;
}

.index-year > h1 {
  margin: 3rem 0 1rem;
}

.index-item {
  margin-top: 0.5rem;
  margin-bottom: 1.5rem;
}

.margin-none {
  margin: 0;
}

.color-secondary {
  color: ◊|color-secondary|;
}

.mono {
  font-family: ◊&[monospace];
}

.margin-bottom-none {
  margin-bottom: 0;
}

.margin-top-none {
  margin-top: 0;
}

.margin-vertical-none,
#logo h1,
figcaption {
  margin-top: 0;
  margin-bottom: 0;
}

.inline {
  display: inline;
}

.flex,
.list {
  display: flex;
}

.baseline {
  align-items: baseline;
}

.flex-item-margin > *,
.list > * {
  margin-right: 0.5rem;
  margin-top: 0;
  margin-bottom: 0.25rem;
}

.post-heading {
  margin-top: 1rem;
  margin-bottom: 4rem;
}

.index-item .date,
.index-item .tags {
  font-family: ◊&[monospace];
  display: inline-block;
  color: ◊|color-secondary|;
  margin: 0 0.5em 0 0;
}

.color-primary {
  color: ◊|color-primary|;
}

/*
* Other widgets */
.badge {
  margin: auto 0.5rem;
}

.page-navigation {
  display: flex;
  flex-direction: column;
  justify-content: space-between;
}

.image-link {
  -webkit-box-shadow: 0px 0px 10px 0px rgba(50, 50, 50, 0.68);
  -moz-box-shadow: 0px 0px 10px 0px rgba(50, 50, 50, 0.68);
  box-shadow: 0px 0px 10px 0px rgba(50, 50, 50, 0.68);
}

.ytembed-default {
  padding-bottom: 50%;
  position: relative;
  overflow: hidden;
}

/*
* Code blocks */
.highlight {
  overflow-x: auto;
  font-family: ◊&[monospace];
  color: ◊|color-primary|;
}

.highlight .linenos .linenodiv {
  padding-right: 0.25rem;
  border-right: 0.1rem solid;
  margin-right: 1rem;
}

.linenodiv pre {
  color: ◊|color-secondary|;
}

/*
* Smaller screen size */
@media (max-width: 1200px) {
  body {
    margin: 5% 5%;
  }
}

/* Local Variables: */
/* eval: (outline-minor-mode) */
/* End: */
