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
CONSERVE_STATUS="__CONSERVE_"$(grep "conserves: " ${TEMPFILE} | sed -e "s/.*: //")

m4 --define="__A"="${A}" \
    --define="__B"="${B}" \
    --define="__EPS"="${EPS}" \
    --define="__CONSERVE_STATUS"="${CONSERVE_STATUS}" \
    --define="__LABEL"="${RESULTS}" \
    results.tpl.tex

rm ${TEMPFILE}