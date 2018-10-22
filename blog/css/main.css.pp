#lang pollen
◊(require racket/string)
◊(define cjk-fallback
   ◊string-append{'Noto Sans CJK TC',
                  'Microsoft Jhenghei',
                  'Microsoft Yahei',
                  'Meiryo',
                  'Malgun Gothic'})
◊(define sans-serif
   ◊string-append{'Fira Sans',
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
◊(define title "h1,h2,h3,h4,h5,h6,h7,.title")

body {
    line-height: 1.6;
    font-family: ◊|sans-serif|;
    font-weight: 400;
    color: #444;
    text-rendering: optimizeLegibility;
}

footer {
    margin-top: 2rem;
}

html, body {
    background-color: #fbfbfb;
}

#logo img {
    max-height: 2em;
    margin-right: 0.4em;
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

.index-item {
    margin-top: 0.5rem;
    margin-bottom: 0.5rem;
}

.index-header .title,
.post-header .title {
    font-size: 1.5rem;
}

.text-primary {
    color: #333 !important;
}

a.text-primary:hover, a.text-primary:focus {
    color: #d0a3ff !important;
}

article header .date-and-category,
article header .tags {
    font-family: ◊|monospace|;
    display: block;
    color: #888;
    margin-bottom: 0;
    margin-top: 0;
    font-size: 1rem;
}

.highlight {
    overflow-x: auto;
    font-family: ◊|monospace|;
    color: black;
}

h1, h2, h3, h4, h5, h6 {
    font-size: 1.5rem;
    line-height: 1.2;
    margin-top: 1.5rem;
    margin-bottom: .5rem;
    font-weight: 300;
    letter-spacing: normal;
}

.project-title {
    color: #444;
    font-weight: 600;
    margin-bottom: 0;
}

.project-desciption {
    color: #555;
}

.project-list {
    column-count: 2;
    list-style: none;
    padding-left: 0;
}

h1 { font-weight: 600; }
h2 { font-weight: 400; }

.index-year {
    font-size: 1.5rem;
    margin-top: 1.5rem;
    margin-bottom: .5rem;
    font-weight: 500;
    ◊; bit of a magic number…
    margin-left: -1.29rem;
}

.index-year::before {
    font-family: "Overpass Mono";
    content: "* ";
    font-size: 1.5rem;
    margin-right: -0.5rem;
    color: #ccc;
}

.collapsed::after {
    content: "...";
}
