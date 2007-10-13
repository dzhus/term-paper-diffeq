DOCNAME := report
${DOCNAME}_depend := $(shell texdepend -print=i -format=make report.tex \
			     | head -n 1 \
			     | sed -e "s/^.*= //")

ifneq (${MAKELEVEL}, 0)

.SECONDEXPANSION:

.PHONY: doc clean ${SOURCEDIR}

%-full-listing.tex: ${SOURCEDIR}/% source-full-listing.tex.m4 source-full-listing.tpl.tex
	m4 --define=target_source=${SOURCEDIR}/$(subst -full-listing.tex,,$@)\
	   source-full-listing.tex.m4 > $@

%-tag-listing.tex: ${SOURCEDIR}/$$(shell echo $$* | sed -e "s/.*__//") tag-listing.sh tag-listing.tpl.tex get-tag.el
	$(SHELL) tag-listing.sh $(shell echo $* | sed -e "s/__.*//") $< > $@

initial-data.tex: ${INITIAL_DATA_FILE} initial-data.sh initial-data.tpl.tex
	$(SHELL) initial-data.sh ${INITIAL_DATA_FILE} > $@

${DOCNAME}.aux: ${DOCNAME}.tex
	@$(info Rebuilding aux file)
	pdflatex ${DOCNAME}.tex

${DOCNAME}.pdf: ${${DOCNAME}_depend} report.aux
	pdflatex ${DOCNAME}.tex

doc: ${DOCNAME}.pdf

clean:
	@rm -frv `hg status --unknown --no-status`
endif