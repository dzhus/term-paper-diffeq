#!/bin/bash
#
# Extract a single tag from any source file using Emacs Semantic.
# 
# Usage:
# 
#     ./tag-listing.sh TAG-NAME FILE-NAME
#
# Output is LaTeX code with tag source ready for inclusion in
# document using `\input`.


TAG=$1
FILE=$2

SOURCE=$(emacs --batch --load grok-lisp.el \
    --exec "(print-tag-from-file \"${TAG}\" \"${FILE}\")" \
    2> /dev/null \
    | grep -v "[ ]*;; .*" | sed -e "s/^\([ ]*\);;@ /\1;; /")

m4 --define="__SOURCE"="${SOURCE}" \
    tag-listing.tpl.tex