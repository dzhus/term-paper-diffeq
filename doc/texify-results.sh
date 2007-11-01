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

cat ${RESULTS} | tail -n +$(($(grep -n "%%" ${RESULTS} | sed -e "s/:.*//")+1)) \
    > ${TEMPFILE}

# DRY violation follows

A=$(grep "A:" ${TEMPFILE} | sed -e "s/.*: //")
B=$(grep "B:" ${TEMPFILE} | sed -e "s/.*: //")
EPS=$(grep "eps:" ${TEMPFILE} | sed -e "s/.*: //")
METHOD=$(grep "method: " ${TEMPFILE} | sed -e "s/.*: //")
SUCCESS="__CONSERVE_"$(grep "conserves: " ${TEMPFILE} | sed -e "s/.*: //")

m4 --define="__A"="${A}" \
    --define="__B"="${B}" \
    --define="__EPS"="${EPS}" \
    --define="__METHOD"="${METHOD}" \
    --define="__CONSERVE_STATUS"="${SUCCESS}" \
    results.tpl.tex
