define(tmpsourcefile, maketemp(/tmp/sourceXXXXXX))
syscmd(cat target_source\
       | grep -v `"[ ]*;; .*"' \
       | sed -e `"s/^\([ ]*\);;@ /\1;; /"' \
       > tmpsourcefile)
define(__REFNAME__, patsubst(target_source,`source/',)-full-listing)
define(__CAPTION__, Содержание файла patsubst(target_source,`source/',))
define(__SRC__, `include(tmpsourcefile)')
changequote(,)
include(source-full-listing.tpl.tex)