#!/bin/bash
helptext="
$0

usage:
  $0 build
  $0 publish orig dest
  $0 cleanup
"

build () {
    racket make-page-tree.rkt
    raco pollen render index.ptree
}

publish () {
    # $1: origin (likely ./)
    # $2: destination

    # return if not supplied the arguments
    [ -z "$1" ] && return
    [ -z "$2" ] && return

    build
    raco pollen publish "$1" "$2"
}

cleanup () {
    rm "*.html" "category/*.html" "category/*.html.pm"
    raco pollen reset
}

case "$1" in
    (build)
        echo building
        build
        ;;
    (publish)
        echo publishing
        publish "$2" "$3"
        ;;
    (clean|cleanup)
        echo cleaning up
        ;;
    (*)
        echo "$helptext"
        ;;
esac
