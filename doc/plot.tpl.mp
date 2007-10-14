verbatimtex
%&latex
  \documentclass{minimal}
  \usepackage[utf8x]{inputenc}
  \renewcommand{\Im}{\mathop{\mathrm{Im}}\nolimits}
  \renewcommand{\Re}{\mathop{\mathrm{Re}}\nolimits}
  \begin{document}
  etex
  
  
  input graph;

filenametemplate "__METHOD-plot.mps";

beginfig(1);
  draw begingraph(10cm, 7cm);
%    setrange(-0.2,whatever, whatever, whatever);
    gdraw "__RE" withcolor blue*0.65;
    glabel.ulft(btex $\Re$ etex, 1) withcolor blue*0.4;
    gdraw "__IM" withcolor red*0.65+white*0.4;
    glabel.llft(btex $\Im$ etex, 1) withcolor red*0.5;
    frame;
  endgraph;
endfig;