#!/bin/bash
helptext="
$0

usage:
  $0 build: Build the site
  $0 publish [dir]: Build the site and copy to dir (default ~/)
  $0 cleanup [interactive]: Clean up built files
  $0 new <Title>: Add a new post with the title <Title>, then edit it with \$EDITOR ($EDITOR)
  $0 loop: Build, wait for /tmp/trigger to be created, build again, notify, loop.
"

waitfor () {
    # wait for $1 to come to existence then exit
    while true; do
        sleep 1
        if test -e "$1"; then
            break
        fi
    done
}

new () {
    date_full="$(date +%Y-%m-%dT%H:%M:%S)"
    newfile="./blog/posts/${date_full%T*}-$1.md.pp"
    {   echo '#lang pollen'
        echo "    Title: $1"
        echo "    Date: $date_full"
        echo "    Tags: DRAFT"
    } >> "$newfile"
    if test -z "$2"; then
        test -z "$EDITOR" && EDITOR="vi" # use vi if EDITOR is not set
        $EDITOR "$newfile"
    fi
}

build () {
    # Build the site into public/
    raco pollen render blog/ || exit 1
    raco pollen render blog/posts/ || exit 1
    raco pollen render blog/css/ || exit 1
    raco frog --build || exit 1
    # frog doesn't copy other files, do it here
    mkdir -p public/css || exit 1
    cp -r blog/css/*.css public/css/ || exit 1
    cp -r blog/images public/ || exit 1
    cp -r blog/videos public/ || exit 1
    cp CNAME public/ || exit 1
    cp favicon.ico public/ || exit 1
    touch public/.nojekyll
}

loop () {
    build
    while true; do
        waitfor /tmp/trigger
        export POLLEN=$RANDOM
        build
        notify-send "Build complete"
    done
}

publish () {
    if test -n "$1"; then
        dir="$1"
    else
        dir="$HOME"
    fi
    # Frog can already build to another dir
    build || exit 1
    mv public "$dir"
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
        publish "$2"
        ;;
    (clean|cleanup)
        echo cleaning up
        cleanup "$2"
        ;;
    (new)
        test -z "$2" && echo "$helptext" && exit
        new "$2" "$3" # creates $newfile
        echo New post: "$newfile"
        ;;
    (loop)
        echo A new build is triggered if /tmp/trigger is present
        loop
        ;;
    (*)
        echo "$helptext"
        ;;
esac
