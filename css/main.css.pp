#lang pollen
◊file->string["emacs.css"]

◊(require
  racket/string
  racket/file
  css-expr
  "../rkt/css.rkt"
  "../rkt/config.rkt")

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
◊(define color-secondary "#333")
◊(define color-primary "#0d0d0d")
◊(define color-accent "#4d2b82")
◊(define color-accent-strong "#cdadff")

/* Old */
/* #444 */
/* #934def */
/* #ef4da9 */
/* #555 */

/* New */
/* #fcfcfc */
/* #0d0d0d */
/* #ade0ff */
/* #cdadff */
/* #ffade0 */
/* #4d2b82 */

/*
* Base */

body {
  margin: 2em 20%;
  line-height: 1.6;
  font-family: ◊&[sans-serif];
  font-size: 16pt;
  font-weight: 400;
  color: ◊|color-primary|;
  text-rendering: optimizeLegibility;
}

/*
* Utilities */

.disabled {
  color: ◊|color-secondary|;
}

/*
* Site Header */

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
  font-size: 16pt;
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
  justify-content: flex-start;
}
#sitefooter nav a {
  color: ◊|color-secondary|;
  margin: 0.5rem 1rem 0.5rem 0;
  font-size: 1.2rem;
}

/*
* Project */

.project p {
  margin-top: 0;
}

.project-title {
  font-weight: 600;
}

.size-normal {
  font-size: 16pt;
}

.toc * {
  font-size: 16pt;
}

/*
* Builtin tags */

/* css-op-all: "+" h1,h2 h2,h3 => h1+h2,h2+h2,h1+h3,h2+h3 */
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
  font-weight: 700;
  color: ◊|color-primary|;
}

h2 {
  font-weight: 400;
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
  /* -webkit-transition: all 0.4s ease; */
  /* -moz-transition: all 0.4s ease; */
  /* -o-transition: all 0.4s ease; */
  /* -ms-transition: all 0.4s ease; */
  /* transition: all 0.4s ease; */
  text-decoration: none;
}

a:hover {
  color: ◊|color-accent-strong|;
  text-decoration: none;
}

/*
* Reference */
◊(css-expr->css
  (css-expr
   [|div#references|
    #:margin-top 3rem
    [p #:margin-top 0 #:margin-bottom 0]]))
  .div#references {
  color: #555;
}

.ref-desc-id {
  font-family: ◊&[monospace];
}

/*
* Index items */
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

.margin-vertical-none {
  margin-top: 0;
  margin-bottom: 0;
}

.inline {
  display: inline;
}

.flex {
  display: flex;
}

.baseline {
  align-items: baseline;
}

.flex-item-margin * {
  margin-right: 0.5rem;
  margin-top: 0;
  margin-bottom: 0;
}

.index-item .date,
.index-item .tags {
  font-family: ◊&[monospace];
  display: inline-block;
  color: ◊|color-secondary|;
  margin: 0 0.5em 0 0;
  font-size: 16pt;
}

.color-primary {
  color: ◊|color-primary|;
}

.post-heading {
  margin: 1rem 0;
}

.post-heading .title {
  margin: 0;
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
  font-size: 16pt;
}

.image p {
  margin-top: 0;
  margin-bottom: 0;
}
.image-caption {
  text-align: center;
  font-family: ◊&[monospace];
  font-style: italic;
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
  font-size: 16pt;
  overflow-x: auto;
  font-family: ◊&[monospace];
  color: ◊|color-primary|;
}

.highlight .linenos {
  padding-right: 0.3rem;
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
