#!/bin/bash
helptext="
$0

usage:
  $0 build: Build the site
  $0 publish: Build the site and copy to ./public/
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
    [ "$1" == rebuild ] && export POLLEN=$RANDOM
    racket generate-pages.rkt || return 1
    raco pollen render css/main.css.pp || return 1
    if [ "$USER" == travis ]; then
        raco pollen render index.ptree || return 1
    else
        raco pollen setup
        raco pollen render -p
    fi
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
    build || exit 1
    raco pollen publish . "$HOME"/public
    mv ~/public .
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
        build "$2"
        ;;
    (publish)
        echo publishing
        publish
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
