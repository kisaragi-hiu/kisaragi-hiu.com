#!/bin/bash
helptext="
$0

usage:
  $0 build: Build the site
  $0 publish [dir]: Build the site and copy to dir (default ~/)
  $0 cleanup [interactive]: Clean up built files
  $0 loop [rebuild]: Build, wait until /tmp/trigger appears, build again, notify, loop.
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

build () {
    # Build the site into public/
    raco pollen render blog/ || return 1
    raco pollen render blog/posts/ || return 1
    raco pollen render blog/css/ || return 1
    raco frog --build || return 1
    # frog doesn't copy other files, do it here
    mkdir -p public/css || return 1
    cp -r blog/css/*.css public/css/ || return 1
    cp -r blog/images public/ || return 1
    cp -r blog/videos public/ || return 1
    cp CNAME public/ || return 1
    cp favicon.ico public/ || return 1
    touch public/.nojekyll
    # Clean up the tags.json. Needs Racket to know what (find-system-path 'temp-dir) is.
    racket -e "(let ([tags.json (build-path (find-system-path 'temp-dir) \"tags.json\")])
                 (when (file-exists? tags.json)
                   (delete-file tags.json)))"
}

loop () {
    [ "$1" == rebuild ] && rebuild=1
    build
    while true; do
        waitfor /tmp/trigger
        [ "$rebuild" == 1 ] && export POLLEN=$RANDOM # full rebuild
        build || notify-send "Build error"
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
    (loop)
        echo A new build is triggered if /tmp/trigger is present
        loop "$2"
        ;;
    (*)
        echo "$helptext"
        ;;
esac
