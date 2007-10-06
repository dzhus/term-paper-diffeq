.PHONY: listing.tex latex-docs clean

listing.tex: listing.tex.m4 diff-eq.scm
	m4 listing.tex.m4 > listing.tex
latex-docs: report.tex listing.tex diff-eq.scm
	pdflatex report.tex
clean:
	rm -v `hg status --unknown --no-status`