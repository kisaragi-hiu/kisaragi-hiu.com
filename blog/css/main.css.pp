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

#logo img {
    max-height: 2em;
    margin-right: 0.4em;
}

.text-primary {
  color: #333 !important;
}

a.text-primary:hover, a.text-primary:focus {
  color: #d0a3ff !important;
}

