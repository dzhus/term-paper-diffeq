verbatimtex \input{preheader}
\begin{document}
etex;

input graph;

filenametemplate "__METHOD-plot.mps";

beginfig(1);
draw begingraph(12cm, 7cm);
%  setrange(origin, whatever, whatever);
%  autogrid(otick.bot,);
  gdraw "__RE";
  gdraw "__IM";
  frame.llft withcolor red;
endgraph;
endfig;