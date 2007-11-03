#!/bin/bash
#
# Parse results and produce LaTeX code suitable for inclusion in paper.
#
# Usage:
#
#    ./texify-results.sh RESULTS
#    

TEMPFILE=$(mktemp /tmp/docXXXXXX)

RESULTS=$1

# Strip off u(x) approximation as it's not used in the template
cat ${RESULTS} | tail -n +$(($(grep -n "%%" ${RESULTS} | sed -e "s/:.*//")+1)) \
    > ${TEMPFILE}

# DRY violation follows

A=$(grep "A:" ${TEMPFILE} | sed -e "s/.*: //")
B=$(grep "B:" ${TEMPFILE} | sed -e "s/.*: //")

EPS=$(grep "eps:" ${TEMPFILE} | sed -e "s/.*: //")
STATUS="__CONSERVE_"$(grep "conserves: " ${TEMPFILE} | sed -e "s/.*: //")

# Generate a proper plot filename prefix according to one being
# generated in `plot-results.sh`
t=${RESULTS/*__/}
STATEMENT=${t/.scm-results/}
METHOD=${RESULTS/__*/}

PLOT_PREFIX=${METHOD}__${STATEMENT}

m4 --define="__A"="${A}" \
    --define="__B"="${B}" \
    --define="__EPS"="${EPS}" \
    --define="__METHOD"="${METHOD}" \
    --define="__CONSERVE_STATUS"="${STATUS}" \
    --define="__LABEL"="${METHOD}__${STATEMENT}.scm-results" \
    --define="__PLOT_PREFIX"="${PLOT_PREFIX}" \
    results.tpl.tex

rm ${TEMPFILE}