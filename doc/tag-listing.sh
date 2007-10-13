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


SOURCE=$(./extract-tag.sh $1 $2)

m4 --define="__SOURCE"="${SOURCE}" \
    tag-listing.tpl.tex