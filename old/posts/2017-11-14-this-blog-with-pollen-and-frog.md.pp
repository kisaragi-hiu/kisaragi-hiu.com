#lang pollen
    Title: Setting up this site with Pollen and Frog
    Date: 2017-11-14T10:56:43
    Tags: category:Meta, language:en, Racket, Frog, Pollen

I have wanted to keep a personal site since 2015. I started on Jimdo, then moved to WordPress.com, then Blogger, then just let the site rot. I wasn't writing much, and the only posts I had were copies of my UTAU cover announcements.

# Github Pages

In March this year, I discovered Github Pages, and wanted to revive my personal site. I started with the official tutorial, not understanding static site generators or Jekyll, even. I only wrote one post during this time, basically ◊site-crossref["blog/2017-03-19-new-website"]{the hello world post}.

I then moved to Hugo in June, with the next two posts being yet again, cover announcements. I didn't (and still don't) understand Hugo, and so I was in the dark when trying to get the site to look the way I want it to. After July, the site was left to rot again.

# First encounter with Frog, and my Pollen site based on mstill.io

In September, after trying out Pollen for my self introduction in August (when my teacher told us to do one), and looking at Frog for a bit, I tried to move the site onto Frog. Knowing next to nothing of Bootstrap and the Racket html templating system (which Frog utilizes), I was again in the dark in stylizing and designing the site. As I had just played with Pollen, I decided to see if anyone has built a blog using Pollen. I found ◊link["https://mstill.io"]{Malcolm Still's website} on Github, and forked it to use as a template, as it is released under the BSD 3-clause license.

# Back to Frog, preprocessed with Pollen

When trying to customize and extend mstill.io's system, I had no option but to learn how Malcolm Still has implemented it. This actually taught me a lot about how static site generators (generally) work: they take in source files, turn them into HTML, then combine them into templates. Previously I thought of templates as a black box, but now I see they're just combined together like a tree.

Eventually, after a few attempts at adding features to posts, I started facing an increasing maintenance overhead. There were four distinct templates, the site index, the tag index, the tags index (a list of all tags), and the general page template, which had to serve for posts and non-posts. Then there are also some features I have no idea how to do with just Pollen, like pagination. I decided, after two weeks, to move to a proper static site generator.

## Choosing the static site generator

I thought about a few different static site generators. Hugo and Hexo both seemed pretty good, but after seeing Hugo shortcodes and realizing how easy is it to do with Pollen, I decided to stay in the Racket ecosystem and use Frog mainly, maybe with Pollen as the preprocessor. To move back to Frog, I had to rewrite the posts in Markdown, figure out how to deal with Markdown newlines, and figure out a design.

## Port over the posts, preprocess them with Pollen

As it turned out, preprocessing the posts with Pollen actually means I can keep the tag functions I was using, and simply tell them to return an HTML or Markdown string. This way I basically have an extensible equivalent to Hugo's shortcodes: for instance, `◊"◊"github["kisaragi-hiu"]{this link}` gets turned into ◊github["kisaragi-hiu"]{this link}.

## Dealing with Markdown newlines

In Markdown, a single newline doesn't get turned into an actual newline in the output. This generally works fine, except when I want paragraphs with fixed length lines. Splitting each line into its own paragraph is no good, as I still want paragraphs to be clear. I solved this by having a newline-decode function, which turns two or more newlines into one, and adds \<br\> to each line.

## Figuring out the design

This is the trickest part for me. My web development experience only consisted of going through freeCodeCamp's first few turorials and modifying an already styled tenplate from mstill.io.

I started with trying to make a decent looking top bar in Bootstrap, then gave up because I'm overwhelmed by Bootstrap's complexity. I then looked around and tried to base my style on Skeleton CSS, this time succeeding.

In the end I spent two weeks from putting up an issue on the Github page to move away from Pollen, to having an overall better site (compared to the mess I added onto mstill.io's basis) built on Frog, in two weeks time.

# Extra: setting up Travis CI

I followed Alexis King's ◊link["https://lexi-lambda.github.io/blog/2015/07/18/automatically-deploying-a-frog-powered-blog-to-github-pages/"]{blog post} when setting up Travis when I still hadn't moved onto Frog. Instead of using just one deploy.sh though, I have a build.sh with a build and a publish function, and a deploy.sh which does the deployment.

I develop this site on ◊github["kisaragi-hiu/kisaragi-hiu.github.io"]{Github}, with posts released under CC-BY-SA and code under BSD 3-clause.
