define(sourcefile, maketemp(/tmp/sourceXXXXXX))
syscmd(cat diff-eq.scm\
       | grep -v `"[ ]*;; .*"' \
       | sed -e `"s/^\([ ]*\);;@ /\1;; /"' \
       > sourcefile)
changequote(,)
\lstdefinelanguage[guile]{Scheme}
        {morekeywords={define,let,map,lambda,if,cond,or,and,else},
         morecomment=[l]{;},
         morestring=[b]{"}}
\lstset{mathescape=true}
\lstset{language=[guile]Scheme}
%\lstset{numbers=left,stepnumber=10,numberblanklines=false}
\lstset{basicstyle=\footnotesize}
\begin{lstlisting}
include(sourcefile)
\end{lstlisting}