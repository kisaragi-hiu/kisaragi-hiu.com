# kisaragi-hiu.com

[![Build Status](https://travis-ci.org/kisaragi-hiu/kisaragi-hiu.com.svg?branch=source)](https://travis-ci.org/kisaragi-hiu/kisaragi-hiu.com)

My personal website, built with [Pollen](https://github.com/mbutterick/pollen).

## Requirements

See .build.yml.

## Build

```sh
make build
```

# Random documentation bits (most likely specific to this site)

## Markup in `define-meta`

- To use markup in a `◊define-meta[name]{content}` block, the content needs to be wrapped in another tag (eg. a `◊span`).

`◊define-meta[name]{content ◊tag{content}}` gets expanded to `(define-meta name "content" (tag "content"))`, which is invalid syntax.

`◊define-meta[name]{◊span{content ◊tag{content}}}` becomes `(define-meta name (span "content" (tag "content")))`.

## Categories

The canonical list of categories is taken from `index.ptree`, *not* from all blog posts. To add a new category, an entry must be added to `index.ptree`.
