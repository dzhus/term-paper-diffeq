verbatimtex
%&latex
  \documentclass{minimal}
  \usepackage[utf8x]{inputenc}
  \renewcommand{\Im}{\mathop{\mathrm{Im}}\nolimits}
  \renewcommand{\Re}{\mathop{\mathrm{Re}}\nolimits}
  \begin{document}
etex
  
  
input graph;

filenametemplate "__PLOT_PREFIX-plot.mps";

beginfig(1);
  draw begingraph(10cm, 7cm);
%    setrange(-0.2,whatever, whatever, whatever);
    gdraw "__RE" withpen pencircle scaled 1.5 withcolor blue*0.7+white*0.1;
    glabel.ulft(btex $\Re$ etex, 1) withcolor blue*0.5;
    gdraw "__IM" withpen pencircle scaled 1.2 withcolor red*0.7+white*0.1;
    glabel.llft(btex $\Im$ etex, 1) withcolor red*0.5;
    pickup pencircle scaled 1.1;
    frame.llft;
  endgraph;
endfig;