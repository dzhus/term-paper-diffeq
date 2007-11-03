DOCNAME := report

include ${DOCNAME}-deps.mk

ifneq (${MAKELEVEL}, 0)

.PRECIOUS: %-plot.mps %-results

.SECONDEXPANSION:

.PHONY: doc clean ${SOURCEDIR}

%-full-listing.tex: ${SOURCEDIR}/% source-full-listing.sh source-full-listing.tpl.tex
	$(SHELL) source-full-listing.sh $< > $@

%-tag-listing.tex: ${SOURCEDIR}/$$(shell echo $$* | sed -e "s/.*__//") \
                   tag-listing.sh tag-listing.tpl.tex grok-lisp.el
	$(SHELL) tag-listing.sh $(shell echo $* | sed -e "s/__.*//") $< > $@

%-initial-data.tex: ${SOURCEDIR}/% initial-data.sh initial-data.tpl.tex
	$(SHELL) initial-data.sh $< > $@

%-results: ${SOURCEDIR}/$$(shell echo $$* | sed -e "s/.*__//") \
           ${SOURCEDIR}/$$(shell echo $$* | sed -e "s/__.*//")-solution.scm \
           ${SHARED_SOURCES} results.sh
	$(SHELL) results.sh $< $(shell echo $* | sed -e "s/__.*//") dispatcher.scm ${SOURCEDIR} > $@

%-plot.mps: %-results plot-results.sh plot.tpl.mp
	$(SHELL) plot-results.sh $<

%-results.tex: %-results %-plot.mps texify-results.sh results.tpl.tex
	$(SHELL) texify-results.sh $< > $@

${DOCNAME}.aux: ${INCLUDES} ${FIGS} ${DOCNAME}.tex
	@echo $<
	pdflatex ${DOCNAME}.tex

${DOCNAME}.pdf: report.aux
	pdflatex ${DOCNAME}.tex

${DOCNAME}-deps.mk: ${DOCNAME}.tex
	texdepend -o $@ -print=if $<

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`
endif
