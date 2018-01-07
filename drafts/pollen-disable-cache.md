Recently Pollen got support for caching, substantially improving build time. However, I realized it doesn't detect changes in files imported with `file->string` (as to be expected), which I'm using to remove duplicated code implementing special tags in `index-template` and `post-template`. Putting how hacky that implementation is aside, I need to force pollen to not use the cache temporarily when I'm editing that template.

Reading through ◊link["http://docs.racket-lang.org/pollen/Cache.html"]{the documentation}, it states that 'The compile cache tracks the modification date of the source file, the current setting of The POLLEN environment variable, and the modification dates of the template and "pollen.rkt" (if they exist).' So, to temporarily force a build without using the cache, I can just set POLLEN to something different than the last build.

◊highlight['bash]{
env POLLEN=$RANDOM raco pollen build
}

Voilà!
