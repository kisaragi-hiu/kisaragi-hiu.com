#!/bin/bash
helptext="
$0

usage:
  $0 build: Build the site
  $0 publish: Build the site then move built files to public/
  $0 cleanup: Clean up built files
  $0 new <Title>: Add a new post with the title <Title>, then edit it with \$EDITOR ($EDITOR)
"

new () {
    date_full="$(date +%Y-%m-%dT%H:%M:%S)"
    newfile="./_src/posts/$date_full-$1.md.pp"
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
    raco pollen render _src/
    raco pollen render _src/posts/
    raco frog --build
}

publish () {
    build || exit 1
    raco pollen publish . ~/public
}

cleanup () {
    if test -n "$1"; then
        git clean -Xdn # dry run
        read -p "Confirm delete (y/n)? " yn
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
