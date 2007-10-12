define(tmpfile, maketemp(/tmp/sourceXXXXXX))
syscmd(cat target_statement\
       | grep -v `"[ ]*;; .*"' \
       > tmpfile)

define(RIGHTBOUND, esyscmd(cat tmpfile \
                               | grep right-bound \
                               | sed -e "s/^.* //" \
                               | tr -d [:space:]))

define(WAVENUMBER, esyscmd(cat tmpfile \
                               | grep wave-number \
                               | sed -e "s/^.* //" \
                               | tr -d [:space:]))

define(NTEX, `esyscmd(cat tmpfile \
                     | grep "$n(x)" \
                     | sed -e "s/;;@ //" \
                     | sed -e "s/\$//g" \
                     | tr -d "\n")')

define(NLISP, `esyscmd(emacs --batch \
                             --load get-tag.el \
                             --exec "(print-tag-from-file \"f\" \"target_statement\")" \
                             &> /dev/stdout)')

include(initial-data.tpl.tex)