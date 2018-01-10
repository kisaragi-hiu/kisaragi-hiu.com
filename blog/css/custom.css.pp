#lang pollen
◊(require racket/string)

◊(define cjk-fallback
   ◊string-append{'Noto Sans CJK TC',
                  'Microsoft Jhenghei',
                  'Microsoft Yahei',
                  'Meiryo',
                  'Malgun Gothic'})

◊(define monospace
   ◊string-append{'Fira Mono',
                  'Noto Sans Mono CJK',
                  ◊|cjk-fallback|,
                  'monospace'})

◊(define title
   ◊string-append{h1,
                  h2,
                  h3,
                  h4,
                  h5,
                  h6,
                  h7,
                  .title})

/* Using Skeleton, 1.0rem = 10px, 1.5rem = 15px, etc. */
/* I'm following a 0.3/0.6 multiplier */

◊import["inline-colors.css"]

body {
    font-family: 'Fira Sans', ◊|cjk-fallback|, sans-serif;
}

◊title {
    font-family: 'Overpass', ◊|cjk-fallback|, sans-serif;
}

code {
    font-family: 'Fira Mono', monospace, ◊|cjk-fallback|;
}

code {
    color: black;
    overflow-x: auto;
}

/* When highlighted code blocks are too wide, they wrap. Resulting in the */
/* line numbers column's rows not lining up with the code rows. Prevent */
/* wrapping. */
pre {
    white-space: pre;
    width: inherit;
}

a,
.date-and-category a {
    color: #a868e8;
    -webkit-transition: all 0.4s ease;
    -moz-transition: all 0.4s ease;
    -o-transition: all 0.4s ease;
    -ms-transition: all 0.4s ease;
    transition: all 0.4s ease;
}

img {
    -webkit-transition: all 0.6s ease;
    -moz-transition: all 0.6s ease;
    -o-transition: all 0.6s ease;
    -ms-transition: all 0.6s ease;
    transition: all 0.6s ease;
}

a:hover {
    color: #d0a3ff;
}

#topheader a,
article header a,
#social-links a {
    color: #444;
    text-decoration: none;
}

#topheader a:hover,
article header a:hover,
#social-links a:hover {
    color: #777;
}

#topheader {
    font-size: 2.4rem;
    margin-top: 1em;
}

#topheader .logo {
    font-weight: 300;
    display: flex;
    justify-content: space-between;
    margin-bottom: 0;
}

#topheader img {
    max-height: 2em;
    margin-right: 0.4em;
}

#topheader nav ul {
    list-style: none;
}

#topheader nav ul li {
    display: inline;
}

#topheader nav ul li a {
    font-size: 2.1rem;
    padding-right: 0.4em;
    text-decoration: none;
}

article {
    font-size: 1.8rem;
}

img {
    /* this used to be article img, but non-post pages are not in <article> */
    max-width: 100%;
    height: auto;
}

a img:hover {
    opacity: 0.8;
}

.image-caption {
    color: #444;
    font-style: italic;
    font-size: 2.0rem;
}

#social-links {
    list-style: none;
}

#social-links li {
    display: inline;
    font-size: 4.8rem;
    margin-right: 6px;
}

article header {
    font-size: 2.4rem;
    margin-bottom: 1rem;
}

/* article header p {
     display: inline;
} */

article header .date-and-category {
    font-family: 'Fira Mono', monospace;
    display: block;
    color: #888;
    margin-bottom: 0;
    margin-top: 0;
    font-size: 1.6rem;
}

.post-header .title{
    font-weight: 600;
}

.pager {
    list-style: none;
}

.pager li {
    display: inline;
}

.pager .next {
    float: right;
}

body {
    color: #444;
    text-rendering: optimizeLegibility;
}

html,
body {
    background-color: #fbfbfb;
}

blockquote {
    font-size: 1.8rem;
    font-style: italic;
    margin-top: 1.0rem;
    margin-bottom: 1.0rem;
    margin-left: 0;
    padding-left: 1.5rem;
    border-left: 0.5rem solid #ccc;
}

p {
    font-size: 100%;
    margin-top: 0rem;
    margin-bottom: 1.2rem;
}

.diff-new {
  color: green;
}

.diff-old {
  color: red;
  text-decoration: line-through;
}

@media print {
    .no-print,
    .no-print * {
        display: none;
    }
}
