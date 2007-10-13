#!/bin/bash
#
# Extract whole source code from Scheme file.
#
# Usage:
#
#     ./source-full-listing.sh FILE-NAME
#
# Output is LaTeX code with source captioned listing ready for
# inclusion using `\input`.

SOURCE=$(cat $1 | grep -v "[ ]*;; .*" | sed -e "s/^\([ ]*\);;@ /\1;; /")

BASENAME=$(basename $1)

m4 --define="__SOURCE"="${SOURCE}" \
    --define="__BASENAME"="${BASENAME}" \
    source-full-listing.tpl.tex