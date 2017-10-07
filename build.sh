#!/bin/bash
helptext="
$0

usage:
  $0 build
  $0 publish
  $0 cleanup
  $0 new <Title>
"

new () {
    date_now="$(date --iso-8601=date)"
    newfile="./post/post-$date_now-$1.html.pm"
    {
        echo '#lang pollen'
        echo "◊define-meta[headline]{$1}"
        echo "◊define-meta[publish-date]{$date_now}"
        echo "◊define-meta[categories]{}"
        echo "◊define-meta[comments]{true}"
    } >> "$newfile"
    $EDITOR "$newfile"
}

build () {
    racket make-page-tree.rkt
    raco pollen render
}

deploy () {
    build
    git commit -am "Deploy"
    git push
}

publish () {
    # $1: origin (likely ./)
    # $2: destination

    # return if not supplied the arguments
    build
    rm -rf ./public/*
    racket ./publish.rkt
}

cleanup () {
    rm ./*.html ./category/*.html ./category/*.html.pm ./post/*.html
    raco pollen reset
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
        cleanup
        ;;
    (new)
        test -z "$2" && echo "$helptext" && exit
        echo New post: "$2"
        new "$2"
        ;;
    (deploy)
        deploy
        ;;
    (*)
        echo "$helptext"
        ;;
esac
