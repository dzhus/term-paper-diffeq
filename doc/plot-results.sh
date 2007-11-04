#!/bin/bash
#
# A script to produce `.mps` MetaPost mpgraph plot using text file
# with results 
#
# Usage:
#
#     ./plot-results.sh RESULTS
#     

TEMPFILE=$(mktemp /tmp/docXXXXXX)

RESULTS=$1

# Strip out everything except u(x) approximation
cat ${RESULTS} | head -n $(($(grep -n "%%" ${RESULTS} | sed -e "s/:.*//")-1)) \
    > ${TEMPFILE}

RE=$(mktemp /tmp/docXXXXXX)
IM=$(mktemp /tmp/docXXXXXX)

# Split Re(u) and Im(u) columns
cut -d' ' -f1,2 ${TEMPFILE} > ${RE}
cut -d' ' -f1,3 ${TEMPFILE} > ${IM}

PLOT_PREFIX=${RESULTS/results/plot}


# Generate `.mps` plot
m4 --define="__PLOT_PREFIX"="${PLOT_PREFIX}" \
    --define="__RE"="${RE}" \
    --define="__IM"="${IM}" \
    plot.tpl.mp > ${TEMPFILE}

mpost -interaction=nonstopmode ${TEMPFILE}
    
rm ${RE} ${IM} ${TEMPFILE}