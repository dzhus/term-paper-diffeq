define(sourcefile, maketemp(/tmp/sourceXXXXXX))
syscmd(cat diff-eq.scm\
       | grep -v `"[ ]*;; .*"' \
       | sed -e `"s/^\([ ]*\);;@ /\1;; /"' \
       > sourcefile)
define(__SRC__, `include(sourcefile)')
changequote(~^,^~)
include(~^fm-full-listing.tpl.tex^~)