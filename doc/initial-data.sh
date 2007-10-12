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

emacs --batch --load get-tag.el \
    --exec "(print-tag-from-file \"f\" \"$1\")" \
    &> ${TEMPFILE}

NLISP=`cat ${TEMPFILE}`

m4 --define="NLISP"="${NLISP}" \
    --define="NTEX"="${NTEX}" \
    --define="RIGHTBOUND"="${RIGHTBOUND}" \
    --define="WAVENUMBER"="${WAVENUMBER}" \
    initial-data.tpl.tex