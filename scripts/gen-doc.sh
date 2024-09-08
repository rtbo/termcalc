#!/usr/bin/env bash

SCRIPTS_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
DOC_DIR=$(dirname $SCRIPTS_DIR)/doc

if ! command -v asciidoctor &> /dev/null
then
    echo -e "asciidoctor could not be found. Please install it first."
    exit 1
fi

echo "Generating man page in $DOC_DIR/gen/tc.1.ansi"

OLD=$(stty -g)
stty cols 80

asciidoctor -b manpage "$DOC_DIR/Manual.adoc" -o "$DOC_DIR/gen/tc.1"

export MAN_KEEP_FORMATTING=1
man "$DOC_DIR/gen/tc.1" > "$DOC_DIR/gen/tc.1.ansi"

stty "$OLD"
