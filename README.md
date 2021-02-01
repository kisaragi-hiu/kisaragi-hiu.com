# kisaragi-hiu.com

[![builds.sr.ht status](https://builds.sr.ht/~kisaragi_hiu/kisaragi-hiu.com/commits/.build.yml.svg)](https://builds.sr.ht/~kisaragi_hiu/kisaragi-hiu.com/commits/.build.yml?)

My personal website.

## Build process

- Preprocess CSS with Pollen
- Build site with Hugo

## Requirements

See .build.yml.

Requires the `path.org` → `path/` rewriting [released in go-org 1.4](https://github.com/niklasfasching/go-org/commit/84d56e95624f3ae1099edb9b527d4eb4a5df4e1d); it’s [not yet available](https://github.com/gohugoio/hugo/commit/212e5e554284bc9368e52a512ed09be5a0224d3e) in a released version of Hugo. As such, we have to use latest Hugo for now (as of 2021-02-01).

## Build

```sh
make public
```

# Random documentation bits

## Pages vs. Blog posts

A blog post has a date and is kind of like a public diary.

An individual page should be a standalone document (ie. [a concept note](https://www.orgroam.com/manual/A-Brief-Introduction-to-the-Zettelkasten-Method.html)); it should be also be a document that will [obviously not fit on Wikipedia](https://www.gwern.net/Wikipedia-and-Other-Wikis).
