DOCNAME := report

include ${DOCNAME}-deps.mk

define get-target-source
$(shell echo $1 | sed -e "s/.*__//")
endef

define get-target-method
$(shell echo $1 | sed -e "s/__.*//")
endef

# Make a list of all result files for given statement file
define all-results
$(foreach method,${ALL_METHODS},${method}__$1-results)
endef

ifneq (${MAKELEVEL}, 0)

.PRECIOUS: %-plot.mps %-results %-all-plots.mps

.SECONDEXPANSION:

.PHONY: doc clean ${SOURCEDIR}

%-full-listing.tex: ${SOURCEDIR}/% \
                    source-full-listing.sh source-full-listing.tpl.tex
	$(SHELL) source-full-listing.sh $< > $@

%-tag-listing.tex: ${SOURCEDIR}/$$(call get-target-source,$$*) \
                   tag-listing.sh tag-listing.tpl.tex grok-lisp.el
	$(SHELL) tag-listing.sh $(shell echo $* | sed -e "s/__.*//") $< > $@

%-initial-data.tex: ${SOURCEDIR}/% initial-data.sh initial-data.tpl.tex
	$(SHELL) initial-data.sh $< > $@

%-results: ${SOURCEDIR}/$$(call get-target-source,$$*) \
           ${SOURCEDIR}/$$(call get-target-method,$$*)-solution.scm \
           ${SHARED_SOURCES} results.sh
	$(SHELL) results.sh $< $(call get-target-method,$*) dispatcher.scm ${SOURCEDIR} > $@

%-all-plots.mps: $(call all-results,%) \
                 plot-results.sh plot.tpl.mp
	$(SHELL) plot-results.sh $(call all-results,$*)

%-plot.mps: %-results plot-results.sh plot.tpl.mp
	$(SHELL) plot-results.sh $<

%-results.tex: %-results %-plot.mps texify-results.sh results.tpl.tex
	$(SHELL) texify-results.sh $< > $@

${DOCNAME}.aux: ${INCLUDES} ${FIGS} ${DOCNAME}.tex
	pdflatex ${DOCNAME}

${DOCNAME}.pdf: report.aux
	pdflatex ${DOCNAME}

${DOCNAME}-deps.mk: ${DOCNAME}.tex
	texdepend -o $@ -print=if $<

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`
endif
