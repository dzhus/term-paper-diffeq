#!/bin/bash
#
# Parse initial data file and call dispatcher code to produce results
# suitable for further processing.
#
# Usage:
#
#     ./results.sh INITIAL-DATA-FILE METHOD DISPATCHER SOURCES-DIR
#     

DATAFILE=$1
METHOD=$2
DISPATCHER=$3
LOAD_PATH=$4

TEMPFILE=$(mktemp /tmp/docXXXXXX)

# Strip out non-TeXish comments

cat $1 | grep -v "[ ]*;; .*" > ${TEMPFILE}

# Extract numeric parameters

RIGHTBOUND=$(cat ${TEMPFILE} | grep right-bound | \
    sed -e "s/^.* //" | tr -d "[:space:]")

WAVENUMBER=$(cat ${TEMPFILE} | grep wave-number | \
    sed -e "s/^.* //" | tr -d "[:space:]")

SUBINTERVALS=$(cat ${TEMPFILE} | grep subintervals | \
    sed -e "s/^.* //" | tr -d "[:space:]")

EPSILON=$(cat ${TEMPFILE} | grep test-epsilon | \
    sed -e "s/^.* //" | tr -d "[:space:]")

guile -L ${LOAD_PATH} -e dispatch ${LOAD_PATH}/${DISPATCHER} \
    -a ${RIGHTBOUND} -k ${WAVENUMBER} -n ${SUBINTERVALS} \
    -t ${EPSILON} -m ${METHOD} --function-file ${DATAFILE}