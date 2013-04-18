
#PDF = $(patsubst %.tex,%.pdf, $(wildcard *.tex))
PDF = pc.pdf pc-ijbc.pdf p3p.pdf

all: $(PDF) p3p.html

$(PDF) : %.pdf : %.tex  Makefile
	pdflatex $<

%.mobi: %.html Makefile
	kindlegen -c1 -verbose $<

# we force the PDF to be made before HTML
# -i to make mathematical symbols italic
# tth needs to know the base file name, which is furnished by the `stem' as -L$*
# -w2 to write XHTML header
%.html: %.tex %.pdf Makefile
	tth -a -i -w2 -e2 -L$* -w2 -e2 < $< > $@

clean:
	rm -f $(PDF) *.aux *.log *.bib *.out *~ pc.mobi pc.html


