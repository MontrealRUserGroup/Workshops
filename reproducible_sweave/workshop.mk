rnwfile = workshop

.SUFFIXES: .tex .pdf .Rnw

PDFS= workshop.pdf

all: $(PDFS)

.Rnw.tex:
	R CMD Sweave $(rnwfile).Rnw

.tex.pdf:
	R CMD pdflatex $(rnwfile).tex
	R CMD pdflatex $(rnwfile).tex

clean:
	rm -rf $(rnwfile).aux $(rnwfile).dvi $(rnwfile).log $(rnwfile).toc \
		$(rnwfile).bak $(rnwfile)~ $(rnwfile).blg $(rnwfile).bbl \
		$(rnwfile).lot $(rnwfile).lof
	rm -rf $(rnwfile).nav $(rnwfile).snm $(rnwfile).out $(rnwfile).pyc
