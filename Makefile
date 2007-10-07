.PHONY: latex-docs clean

%.tex: %.tex.m4 diff-eq.scm
	m4 $@.m4 > $@

latex-docs: report.tex listing.tex fm-solution.tex diff-eq.scm
	pdflatex report.tex

clean:
	rm -v `hg status --unknown --no-status`