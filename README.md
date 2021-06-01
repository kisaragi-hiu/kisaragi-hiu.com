# kisaragi-hiu.com

[![builds.sr.ht status](https://builds.sr.ht/~kisaragi_hiu/kisaragi-hiu.com/commits/.build.yml.svg)](https://builds.sr.ht/~kisaragi_hiu/kisaragi-hiu.com/commits/.build.yml?)

My personal website.

## Build process

- Install packages needed for build:
  - hugo
  - p7zip, for creating the zip file to upload to Netlify
  - nodejs, for npm
  - npm
  - sass (Dart Sass compiled to JavaScript and installed from npm)
- Fetch assets
  - /static/illust from my personal git forge
  - Maybe also external icons in the future
- Preprocess CSS with Sass
- Build site with Hugo

## Requirements

See .build.yml.

Requires the `path.org` → `path/` rewriting [released in go-org 1.4](https://github.com/niklasfasching/go-org/commit/84d56e95624f3ae1099edb9b527d4eb4a5df4e1d).

## Build

```sh
make public
```
