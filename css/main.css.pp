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

.page-navigation {
    display: flex;
    justify-content: space-between;
}

.translate .lang-en, .translate .lang-zh, .translate .lang-ja {
    display: none;
}

.translate .lang-en:lang(en), .translate .lang-zh:lang(zh), .translate .lang-ja:lang(ja) {
    display: unset;
}

footer {
    margin-top: 2rem;
}

p {
    font-size: 1.2em;
}

html, body {
    background-color: #fbfbfb;
}

#logo img {
    max-height: 2rem;
    margin-right: 0.4rem;
    font-size: 1rem;
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
    line-height: 1.6;
    margin-top: 0;
}

.text-primary {
    color: #333 !important;
}

a.text-primary:hover, a.text-primary:focus {
    color: #d0a3ff !important;
}

header .date,
header .category,
header .tags {
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

h1, h2, h3, h4, h5, h6, .index-stream-title {
    font-size: 1.5rem;
    line-height: 1.2;
    margin-top: 1.5rem;
    margin-bottom: .5rem;
    font-weight: 300;
    letter-spacing: normal;
}

h1, .index-stream-title {
    font-weight: 600;
    color: #444;
}

h2 { font-weight: 400; }

.index-stream-title:hover {
    color: #222;
}

.project-title {
    color: #444;
    font-weight: 600;
    margin-bottom: 0;
}

a.project-link {
    color: #555;
}

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

ul.project-list {
    column-count: 2;
    list-style: none;
    padding-left: 0;
}

div.project-list {
    display: flex;
    padding-left: 0;
    padding-bottom: 2em;
    justify-content: space-around;
}

.image-link {
    -webkit-box-shadow: 0px 0px 10px 0px rgba(50, 50, 50, 0.68);
    -moz-box-shadow:    0px 0px 10px 0px rgba(50, 50, 50, 0.68);
    box-shadow:         0px 0px 10px 0px rgba(50, 50, 50, 0.68);
}

.collapse-level-2 {
    font-size: 1.5rem;
    margin-top: 1.5rem;
    margin-bottom: .5rem;
    font-weight: 500;
    /* bit of a magic number… */
    margin-left: -1.29rem;
}

.index-stream-title {
    /* magic number again… */
    margin-left: -1.5rem;
}

.collapse-level-2::before, .index-stream-title::before {
    font-family: "Overpass Mono";
    font-size: 1.5rem;
    margin-right: -0.5rem;
    color: #aaa;
}

.collapse-level-2::before {
    content: "+ ";
}

.index-stream-title::before {
    content: "* ";
}

.collapsed::after {
    content: "...";
}

.ytembed-default {
    padding-bottom: 50%;
    position: relative;
    overflow: hidden;
}

/* Smaller than phablet */
@media (max-width: 600px) {
    .project-list {
        column-count: 1;
    }

    .collapse-level-2 {
        margin-left: 0;
    }

    .index-stream-title {
        margin-left: 0;
    }
}
