#!/usr/bin/env bash

SCRIPTS_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
DOC_DIR=$(dirname $SCRIPTS_DIR)/doc

echo "Generating man page in $DOC_DIR/gen/tc.1.ansi"

OLD=$(stty -g)
stty cols 80

asciidoctor -b manpage "$DOC_DIR/Manual.adoc" -o "$DOC_DIR/gen/tc.1"

unset MAN_KEEP_FORMATTING
man "$DOC_DIR/gen/tc.1" > "$DOC_DIR/gen/tc.1.txt"
export MAN_KEEP_FORMATTING=1
man "$DOC_DIR/gen/tc.1" > "$DOC_DIR/gen/tc.1.ansi"

gzip -f "$DOC_DIR/gen/tc.1"

stty "$OLD"
