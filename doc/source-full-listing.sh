#!/bin/bash
#
# Extract whole source code from Scheme file.
#
# Usage:
#
#     ./source-full-listing.sh FILE-NAME
#
# Output is LaTeX code with source captioned listing ready for
# inclusion using `\input`. Reference to listing is also provided (use
# `\ref{FILE-NAME-full-listing}`).

SOURCE=$(cat $1 | grep -v "[ ]*;; .*" | sed -e "s/^\([ ]*\);;@ /\1;; /")

CAPTION="Содержимое файла "$(basename $1)
REFNAME=$(basename $1)

m4 --define="__SOURCE"="${SOURCE}" \
    --define="__REFNAME"="${REFNAME}" \
    --define="__CAPTION"="${CAPTION}" \
    source-full-listing.tpl.tex