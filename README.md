# kisaragi-hiu.com

[![builds.sr.ht status](https://builds.sr.ht/~kisaragi_hiu/kisaragi-hiu.com/commits/.build.yml.svg)](https://builds.sr.ht/~kisaragi_hiu/kisaragi-hiu.com/commits/.build.yml?)

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

## Adding a new page that isn’t a blog post

Add a new Org file at project root and remember to add it into `index.ptree` so that Pollen knows to render it.

## Pages vs. Blog posts

A blog post has a date and is kind of like a public diary.

An individual page should be a standalone document (ie. [a concept note](https://www.orgroam.com/manual/A-Brief-Introduction-to-the-Zettelkasten-Method.html)); it should be also be a document that will [obviously not fit on Wikipedia](https://www.gwern.net/Wikipedia-and-Other-Wikis).
