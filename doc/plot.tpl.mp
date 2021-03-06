% m4 macro definitions
define(`__PEN_single', `pencircle scaled 1.5')
define(`__PEN_multi', `pencircle scaled 0.8')

verbatimtex
%&latex
  \documentclass{minimal}
  \usepackage[utf8x]{inputenc}
  \renewcommand{\Im}{\mathop{\mathrm{Im}}\nolimits}
  \renewcommand{\Re}{\mathop{\mathrm{Re}}\nolimits}
  \begin{document}
etex;

input graph;

filenametemplate "__PLOT_PREFIX.mps";

beginfig(1);
  draw begingraph(10cm, 6cm);
    for file = __DATA:
      color real_color, imag_color;
      path real_plot, imag_plot;
    
      real_color = blue*(0.5+uniformdeviate(5)/10)+green*uniformdeviate(2)/10+white*uniformdeviate(15)/100;
      imag_color = red*(0.5+uniformdeviate(5)/10)+white*uniformdeviate(15)/100;
      
      gdata(file, f,
        augment.real_plot(f1, f2);
        augment.imag_plot(f1, f3););
      
      gdraw real_plot withpen __PEN withcolor real_color;
      __LABEL(glabel.ulft(btex $\Re$ etex, 1) withcolor real_color*0.8;)
      
      gdraw imag_plot withpen  __PEN withcolor imag_color;
      __LABEL(glabel.llft(btex $\Im$ etex, 1) withcolor imag_color*0.8;)

    endfor;
    pickup pencircle scaled 1.1;
    frame.llft;
  endgraph;
endfig;