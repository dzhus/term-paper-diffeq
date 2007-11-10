#!/bin/bash
#
# A script to produce `.mps` MetaPost mpgraph plot using text file
# with results 
#
# Usage:
#
#     ./plot-results.sh RESULTS-LIST
#
# If RESULTS-LIST is single file
# `fundmatrix__exo-statement.scm-results`, this script will produce
# `fundmatrix__exo-statement.scm-plot.mps`. In case of several file
# names passed as command line arguments:
# `fundmatrix__statement.scm-results
# iterative-solve__statement.scm-results`
# `statement.scm-all-plots.mps` is a filename of resulting image.

RESULTS=$@

MP=$(mktemp /tmp/docXXXXXX)
DATAFILES=""

# Generate a list of source data files
for file in ${RESULTS}
do
    TEMPFILE=$(mktemp /tmp/docXXXXXX)
    # Strip out everything except u(x) approximation
    cat ${file} \
        | head -n $(($(grep -n "%%" ${file} | sed -e "s/:.*//")-1)) \
        > ${TEMPFILE}
    DATAFILES="${DATAFILES}\"${TEMPFILE}\","
done
DATAFILES=${DATAFILES%,}

# Generate `method__statement.scm-plot` (if one argument passed) or
# `statement.scm-all-plots`. Place labels on plot curves only if one
# result is being drawn
if (( $#==1 ))
then
    PLOT_PREFIX=${RESULTS/results/plot}
    LABEL="\`\$1'"
    PEN="single"
else
    # Warning: script uses name of statement file of the _first_
    # argument in the name of resulting MPS image.
    PLOT_PREFIX=${1/*__/}
    PLOT_PREFIX=${PLOT_PREFIX%-results}"-all-plots"
    LABEL=""
    PEN="multi"
fi


# Generate `.mps` plot
m4 --define="__PLOT_PREFIX"="${PLOT_PREFIX}" \
    --define="__LABEL"="${LABEL}" \
    --define="__DATA"="${DATAFILES}" \
    --define="__PEN"="__PEN_${PEN}" \
    plot.tpl.mp > ${MP}

mpost -interaction=nonstopmode ${MP}

rm ${MP} $(echo ${DATAFILES} | sed -e "s/\"//g" | sed -e "s/,/ /g")
