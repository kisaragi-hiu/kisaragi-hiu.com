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
    racket generate-pages.rkt
    raco pollen render index.ptree || return 1
}

loop () {
    [ "$1" == rebuild ] && rebuild=1
    build
    while true; do
        waitfor /tmp/trigger
        [ "$rebuild" == 1 ] && export POLLEN=$RANDOM # full rebuild
        if build; then
            echo "build.sh: complete"
            notify-send "Build complete"
        else
            echo "build.sh: error"
            notify-send "Build error"
        fi
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
