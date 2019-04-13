#!/bin/bash
helptext="
$0

usage:
  $0 new <Title>
    Add a new draft with the title <Title>.
  $0 publish <filename>
    Publish <filename>, filling in publish date and special tags, and
    adding date to the filename."

new () {
    [ -z "$1" ] && return
    title="$1"
    newfile="$title.html.pm"
    {   echo '#lang pollen'
        echo "◊define-meta[title]{$title}"
        echo "◊define-meta[date]{◊date◊}"
        echo "◊define-meta[category]{◊cat◊}"
        echo "◊define-meta[language]{◊lang◊}"
    } >> "$newfile"
    echo "$newfile"
}

publish () {
    [[ -z "$1" ]] && return
    file="$1" # no date
    date_full="$(date +%Y-%m-%dT%H:%M:%S)"
    date_truncated="${date_full%T*}"

    if [[ -n "$2" ]]; then
        language="$2"
    else
        read -r -p "language (en or zh-tw?): " language
    fi

    if [[ -n "$3" ]]; then
        category="$3"
    else
        read -r -p "category: " category
    fi

    # read -r -p "tags (same format in file): " tags
    tags="" # tags are not yet implemented
    # shellcheck disable=SC2002 # Use sed as a filter.
    cat "$file" \
    | sed s/◊date◊/"$date_full"/g \
    | sed s/◊lang◊/"$language"/g \
    | sed s/◊cat◊/"$category"/g \
    | sed s/◊tags◊/"$tags"/g \
    > blog/"$date_truncated"-"$file"
}

case "$1" in
    new)
        new "$2"
        ;;
    publish)
        publish "$2" "$3" "$4"
        ;;
    *)
        echo "$helptext"
        ;;
esac
