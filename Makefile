.PHONY: latex-docs clean

%-full-listing.tex: %.scm source-full-listing.tex.m4 source-full-listing.tpl.tex
	m4 --define=target_source=$(subst -full-listing.tex,.scm,$@) source-full-listing.tex.m4 > $@

latex-docs: report.tex fundmatrix-full-listing.tex fm-solution.tex
	pdflatex report.tex

clean:
	rm -v `hg status --unknown --no-status`