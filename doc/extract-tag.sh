#!/bin/bash
#
# Extract a single tag from any source file using Semantic in Emacs.
# Usage:
# ./extra-tag.sh TAG-NAME FILE-NAME
#
# Output is full tag raw source.


TEMPFILE=`mktemp /tmp/tagXXXXXX`

emacs --batch --load get-tag.el \
    --exec "(print-tag-from-file \"$1\" \"$2\")" \
    &> ${TEMPFILE}

# Strip nonrelevant Emacs messages
cat ${TEMPFILE} | tail -n +$((`grep -n "^%%$" ${TEMPFILE}| sed -e "s/:.*//"`+1))