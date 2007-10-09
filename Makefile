SOURCEDIR := source
DOCDIR := doc
FULL_LISTINGS := $(foreach file,\
			   $(wildcard source/*.scm),\
			   $(subst ${SOURCEDIR}/,,${file})-full-listing.tex)

SOLUTIONS := fm-solution.tex
INITIAL_DATA_FILE := ${SOURCEDIR}/statement.scm

.PHONY: report.tex clean

%-full-listing.tex: ${SOURCEDIR}/% source-full-listing.tex.m4 source-full-listing.tpl.tex
	m4 --define=target_source=${SOURCEDIR}/$(subst -full-listing.tex,,$@)\
	   source-full-listing.tex.m4 > $@

initial-data.tex: ${INITIAL_DATA_FILE} initial-data.tex.m4 initial-data.tpl.tex
	m4 --define=target_statement=$< \
	   initial-data.tex.m4 > $@

report.aux: report.tex
	@echo "Rebuilding aux file"
	pdflatex report.tex

report.pdf: report.tex report.aux ${FULL_LISTINGS} ${SOLUTIONS} initial-data.tex
	pdflatex report.tex

clean:
	rm -v `hg status --unknown --no-status`