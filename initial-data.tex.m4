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

define(NLISP, `esyscmd(cat target_statement \
                       | grep -v "^;;" \
                       | grep "(\|)")')

include(initial-data.tpl.tex)