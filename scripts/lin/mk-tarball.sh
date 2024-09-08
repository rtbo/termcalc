#!/usr/bin/env bash

source "$( dirname "${BASH_SOURCE[0]}" )/dirs.sh"

DIST_DIR=$TARGET_DIR/dist
TMP_DIR=$DIST_DIR/tmp

if [ ! -f $MAN_FILE ]
then
    echo -e "Man page $MAN_FILE not found. Please run mk-man.sh first."
    exit 1
fi

if [ ! -f $EXE_FILE ]
then
    echo -e "Executable $EXE_FILE not found. Please run `cargo build --release` first."
    exit 1
fi

TAG=$(git describe --abbrev=0 --tags | sed --expression='s/^v//')
ARCH=$(uname -m)
AR_FILE=$DIST_DIR/termcalc-$TAG-linux-$ARCH.tar.gz

pushd $TC_DIR
    cargo build --release
popd

rm -rf $TMP_DIR
mkdir -p $TMP_DIR/bin
mkdir -p $TMP_DIR/share/man/man1

cp $TARGET_DIR/release/tc $TMP_DIR/bin/termcalc
cp $MAN_FILE $TMP_DIR/share/man/man1
gzip -f $TMP_DIR/share/man/man1/termcalc.1

rm $AR_FILE
echo "Creating $AR_FILE"

pushd $TMP_DIR
    tar -czvf $AR_FILE * 
popd

rm -rf $TMP_DIR
