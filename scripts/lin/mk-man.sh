#!/usr/bin/env bash

source "$( dirname "${BASH_SOURCE[0]}" )/dirs.sh"

if ! command -v asciidoctor &> /dev/null
then
    echo -e "asciidoctor could not be found. Please install it first."
    exit 1
fi

echo "Generating man page $MAN_FILE"

asciidoctor -b manpage "$DOC_DIR/Manual.adoc" -o "$MAN_FILE"
