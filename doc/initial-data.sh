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

cat $1 | grep -v "[ ]*;; .*" > ${TEMPFILE}

# Extract numeric parameters

RIGHTBOUND=$(cat ${TEMPFILE} | grep right-bound | \
    sed -e "s/(define right-bound \(.*\))/\1/")

WAVENUMBER=$(cat ${TEMPFILE} | grep wave-number | \
    sed -e "s/(define wave-number \(.*\))/\1/")

NTEX=$(cat ${TEMPFILE} | grep ';;@ \$n(x)' | \
    sed -e "s/;;@ //" | sed -e 's/^\$\(.*\)\$$/\1/g' | tr -d "\n")

# Extract Lisp definition

NLISP=$(./tag-listing.sh f $1)

m4 --define="__NLISP"="${NLISP}" \
    --define="__NTEX"="${NTEX}" \
    --define="__RIGHTBOUND"="${RIGHTBOUND}" \
    --define="__WAVENUMBER"="${WAVENUMBER}" \
    initial-data.tpl.tex