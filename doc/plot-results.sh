#!/bin/bash
#
# A script to produce `.mps` MetaPost mpgraph plot using text file
# with results 
#
# Usage:
#
#     ./plot-results.sh RESULTS
#
# If RESULTS is `fundmatrix__exo-statement.scm-results`, the script
# will produce `fundmatrix__exo-statement.scm-plot.mps`.

RESULTS=$1

TEMPFILE=$(mktemp /tmp/docXXXXXX)
MP=$(mktemp /tmp/docXXXXXX)

# Strip out everything except u(x) approximation
cat ${RESULTS} | head -n $(($(grep -n "%%" ${RESULTS} | sed -e "s/:.*//")-1)) \
    > ${TEMPFILE}

PLOT_PREFIX=${RESULTS/results/plot}

# Generate `.mps` plot
m4 --define="__PLOT_PREFIX"="${PLOT_PREFIX}" \
    --define="__DATA"="${TEMPFILE}" \
    plot.tpl.mp > ${MP}

mpost -interaction=nonstopmode ${MP}
    
rm ${TEMPFILE} ${MP}