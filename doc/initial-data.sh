#!/bin/bash

TEMPFILE=`mktemp /tmp/tagXXXXXX`

cat $1 | grep -v "[ ]*;; .*" > ${TEMPFILE}

# Extract numeric parameters

RIGHTBOUND=`cat ${TEMPFILE} | grep right-bound | \
    sed -e "s/^.* //" | tr -d "[:space:]"`

WAVENUMBER=`cat ${TEMPFILE} | grep wave-number | \
    sed -e "s/^.* //" | tr -d "[:space:]"`

NTEX=`cat ${TEMPFILE} | grep ';;@ \$n(x)' | \
    sed -e "s/;;@ //" | sed -e 's/^\$\(.*\)\$$/\1/g' | tr -d "\n"`

# Extract Lisp definition

NLISP=`./tag-listing.sh f $1`

m4 --define="__NLISP"="${NLISP}" \
    --define="__NTEX"="${NTEX}" \
    --define="__RIGHTBOUND"="${RIGHTBOUND}" \
    --define="__WAVENUMBER"="${WAVENUMBER}" \
    initial-data.tpl.tex