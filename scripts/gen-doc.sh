#!/usr/bin/env bash

SCRIPTS_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
DOC_DIR=$(dirname $SCRIPTS_DIR)/doc

echo "Generating man page in $DOC_DIR/gen/tc.1.ansi"

OLD=$(stty -g)
stty cols 80

pandoc -s -t man "$DOC_DIR/Manual.md" -o "$DOC_DIR/gen/tc.1"

export MAN_KEEP_FORMATTING=1
man "$DOC_DIR/gen/tc.1" > "$DOC_DIR/gen/tc.1.ansi"

gzip -f "$DOC_DIR/gen/tc.1"

stty "$OLD"
