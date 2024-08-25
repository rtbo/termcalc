#! /bin/bash

SCRIPTS_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
TC_DIR="$(dirname $SCRIPTS_DIR)"
DOC_DIR="$TC_DIR/doc"

$SCRIPTS_DIR/gen-doc.sh

cargo build --release

cargo deb
