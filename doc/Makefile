DOCNAME := report
${DOCNAME}_depend := $(shell texdepend -print=i -format=make ${DOCNAME}.tex \
			     | head -n 1 \
			     | sed -e "s/^.*= //")
ifneq (${MAKELEVEL}, 0)

.PRECIOUS: %-plot.mps

.SECONDEXPANSION:

.PHONY: doc clean ${SOURCEDIR}

%-full-listing.tex: ${SOURCEDIR}/% source-full-listing.sh source-full-listing.tpl.tex
	$(SHELL) source-full-listing.sh $< > $@

%-tag-listing.tex: ${SOURCEDIR}/$$(shell echo $$* | sed -e "s/.*__//") tag-listing.sh tag-listing.tpl.tex get-tag.el
	$(SHELL) tag-listing.sh $(shell echo $* | sed -e "s/__.*//") $< > $@

initial-data.tex: ${INITIAL_DATA_FILE} initial-data.sh initial-data.tpl.tex
	$(SHELL) initial-data.sh $< > $@

%-results: ${INITIAL_DATA_FILE} $(wildcard ${SOURCEDIR}/*.scm) results.sh
	$(SHELL) results.sh $< $* dispatcher.scm ${SOURCEDIR} > $@

%-plot.mps: %-results plot-results.sh plot.tpl.mp
	$(SHELL) plot-results.sh $<

%-results.tex: %-results %-plot.mps texify-results.sh results.tpl.tex
	$(SHELL) texify-results.sh $< > $@

${DOCNAME}.aux: ${${DOCNAME}_depend} ${DOCNAME}.tex
	@$(info Rebuilding aux file)
	pdflatex ${DOCNAME}.tex

${DOCNAME}.pdf: report.aux
	@$(info Depends on $<)
	pdflatex ${DOCNAME}.tex

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`
endif