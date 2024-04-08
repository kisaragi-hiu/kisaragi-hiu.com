# kisaragi-hiu.com

My personal website.

## Build process

On push, GitHub Actions is triggered to:

- Set up the repository (checkout, install stuff)
- Generate the Vercel configuration, as I wanted to automate some stuff
- Use the Vercel CLI to build within the GitHub Actions runner and deploy

This dance is necessary as:

- I want to use Vercel for the ability to redirect subpaths to other projects

  > *TL;DR*: Vercel allows me to host subprojects like
  > 
  > - subproject A: kisaragi-hiu.com/subproject-a → example-1aLyX9xX8.vercel.app
  > - subproject B: kisaragi-hiu.com/subproject-b → example-T7nmDoVGB.vercel.app
  > - parent: kisaragi-hiu.com
  > 
  > without adding an ugly client side redirect snippet to every single subproject [which is needed with Netlify].
  >
  > — https://kisaragi-hiu.com/switching-to-vercel
  
- I don't want to build on Vercel's servers
- Vercel's solution for building elsewhere is ~~fucking mental~~ unintuitive, as the CLI wants to handle the build process as well, instead of building on your own and using the CLI just to deploy.
- vercel.json is only automated for Next.js afaik 🙁

## Build locally

- Install packages needed for build: Hugo, Bun, then `bun install`
- Fetch assets
  - /static/illust from my personal git forge
  - Maybe also external icons in the future
- `make public`
- To get a dev server, `make dev`
