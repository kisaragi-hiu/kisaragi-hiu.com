#!/bin/bash
helptext="
$0

usage:
  $0 build: Build the site
  $0 publish: Build the site then move built files to public/
  $0 cleanup [interactive]: Clean up built files
  $0 new <Title>: Add a new post with the title <Title>, then edit it with \$EDITOR ($EDITOR)
"

new () {
    date_full="$(date +%Y-%m-%dT%H:%M:%S)"
    newfile="./blog/posts/${date_full%T*}-$1.md.pp"
    {   echo '#lang pollen'
        echo "    Title: $1"
        echo "    Date: $date_full"
        echo "    Tags: DRAFT"
    } >> "$newfile"
    if test -z "$2"; then
        test -z "$EDITOR" && EDITOR=vi # use vi if EDITOR is not set
        $EDITOR "$newfile"
    fi
}

build () {
    # Build the site into public/
    raco pollen render blog/ || exit 1
    raco pollen render blog/posts/ || exit 1
    raco pollen render blog/css/ || exit 1
    raco frog --build || exit 1

    mkdir -p public/css || exit 1
    cp -r blog/css/*.css public/css/ || exit 1
    cp -r blog/images public/ || exit 1
    cp CNAME public/ || exit 1
    cp favicon.ico public/ || exit 1
    touch public/.nojekyll
}

publish () {
    # Frog can already build to another dir
    build || exit 1
    mv public ~/
}

cleanup () {
    if test -n "$1"; then
        git clean -Xdn # dry run
        read -r -p "Confirm delete (y/n)? " yn
        case "$yn" in
            y*) true;; # Do nothing
            *) exit;;
        esac
    fi
    # X: only ignored; d: include dirs; f: yes I mean it
    git clean -Xdf
}

case "$1" in
    (""|build) # automatically build if not given an option
        echo building
        build
        ;;
    (publish)
        echo publishing
        publish
        ;;
    (clean|cleanup)
        echo cleaning up
        cleanup "$2"
        ;;
    (new)
        test -z "$2" && echo "$helptext" && exit
        echo New post: "$2"
        new "$2" "$3"
        ;;
    (*)
        echo "$helptext"
        ;;
esac
