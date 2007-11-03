#!/bin/bash
#
# Parse initial data file and call dispatcher code to produce results
# suitable for further processing.
#
# Usage:
#
#     ./results.sh STATEMENT METHOD DISPATCHER SOURCES-DIR
#     

STATEMENT=$1
METHOD=$2
DISPATCHER=$3
LOAD_PATH=$4

guile -L ${LOAD_PATH} \
    -e dispatch ${LOAD_PATH}/${DISPATCHER} \
    --method ${METHOD} \
    --statement-file ${STATEMENT}