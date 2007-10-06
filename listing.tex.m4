define(sourcefile, maketemp(/tmp/sourceXXXXXX))
define(taglist, maketemp(/tmp/tagsXXXXXX))
syscmd(cat diff-eq.scm | grep -v `"^[ ]*;.*$"' > sourcefile)
syscmd(etags --language=scheme sourcefile -o - \
             | tail -n +3 \
             | sed -e `"s/(define (//"' \
             | sed -e `"s/ .*$//"' \
             | sed -e `"s/$/,/"' > taglist)
changequote(,)
%\lstset{language=Lisp}
\lstset{emph={include(taglist)}, emphstyle=\underbar,
        emph={[2]define,let,map,lambda,if,cond,or,and,else}, emphstyle=[2]\textbf}
\begin{lstlisting}
include(sourcefile)
\end{lstlisting}