# kisaragi-hiu.com

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
- CSS written using Tailwind
- Build site with Hugo

## Requirements

See `Makefile`.

## Build

```sh
make public
```
