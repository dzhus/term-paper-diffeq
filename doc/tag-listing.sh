#!/bin/bash

SOURCE=$(./extract-tag.sh $1 $2)

m4 --define="__SOURCE"="${SOURCE}" \
    tag-listing.tpl.tex