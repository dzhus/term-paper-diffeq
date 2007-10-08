define(tmpsourcefile, maketemp(/tmp/sourceXXXXXX))
syscmd(cat target_source\
       | grep -v `"[ ]*;; .*"' \
       | sed -e `"s/^\([ ]*\);;@ /\1;; /"' \
       > tmpsourcefile)
define(__SRC__, `include(tmpsourcefile)')
changequote(,)
include(source-full-listing.tpl.tex)