#!/bin/bash
#
# Extract a single tag from any source file using Emacs Semantic.
# 
# Usage:
# 
#     ./extra-tag.sh TAG-NAME FILE-NAME
#
# Output is full tag raw source.


TEMPFILE=$(mktemp /tmp/tagXXXXXX)

emacs --batch --load get-tag.el \
    --exec "(print-tag-from-file \"$1\" \"$2\")" \
    2> /dev/null