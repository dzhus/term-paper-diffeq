#!/bin/bash
#
# Provide `initial-data.tpl.tex` with necessary macro definitions
# extracted from statement file.
# 
# Usage:
# 
#     ./initial-data.sh STATEMENT-FILE
#
# Output is a LaTeX code ready for inclusion using `\input`.


TEMPFILE=$(mktemp /tmp/docXXXXXX)

# Strip out non-TeXish comments

cat ${1} | grep -v "[ ]*;; .*" > ${TEMPFILE}

# Extract numeric parameters

RIGHTBOUND=$(cat ${TEMPFILE} | grep right-bound | \
    sed -e "s/(define right-bound \(.*\))/\1/")

NTEX=$(cat ${TEMPFILE} | grep ';;@ \$n(x)' | \
    sed -e "s/;;@ //" | sed -e 's/^\$\(.*\)\$$/\1/g' | tr -d "\n")

LABEL=$(basename ${1})"-initial-data"

m4 --define="__NTEX"="${NTEX}" \
    --define="__RIGHTBOUND"="${RIGHTBOUND}" \
    --define="__LABEL"="${LABEL}" \
    initial-data.tpl.tex