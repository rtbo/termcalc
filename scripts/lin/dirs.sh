
LIN_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
SCRIPTS_DIR=$(dirname $LIN_DIR)
TC_DIR=$(dirname $SCRIPTS_DIR)
TARGET_DIR="$TC_DIR/target"
EXE_FILE="$TARGET_DIR/release/tc"
DOC_DIR="$TC_DIR/doc"
MAN_DIR="$TARGET_DIR/man"
MAN_FILE="$MAN_DIR/termcalc.1"
