verbatimtex
%&latex
  \documentclass{minimal}
  \usepackage[utf8x]{inputenc}
  \renewcommand{\Im}{\mathop{\mathrm{Im}}\nolimits}
  \renewcommand{\Re}{\mathop{\mathrm{Re}}\nolimits}
  \begin{document}
etex;
  
  
input graph;

filenametemplate "__PLOT_PREFIX-plot.mps";

beginfig(1);
  draw begingraph(10cm, 7cm);
    color real_color, imag_color;
    real_color = blue*(0.5+uniformdeviate(4)/10)+white*uniformdeviate(10)/100;
    imag_color = red*(0.5+uniformdeviate(4)/10)+white*uniformdeviate(10)/100;
    
    gdraw "__RE" withpen pencircle scaled 1.5 withcolor real_color;
    glabel.ulft(btex $\Re$ etex, 1) withcolor real_color*0.8;
    gdraw "__IM" withpen pencircle scaled 1.2 withcolor imag_color;
    glabel.llft(btex $\Im$ etex, 1) withcolor imag_color*0.8;

    pickup pencircle scaled 1.1;
    frame.llft;
  endgraph;
endfig;