
#PDF = $(patsubst %.tex,%.pdf, $(wildcard *.tex))
PDF = pc.pdf pc-ijbc.pdf p3p.pdf

all: $(PDF)

$(PDF) : %.pdf : %.tex  Makefile
	pdflatex $<

pc.mobi: pc.html Makefile
	kindlegen -c1 -verbose $<

pc.html: pc.tex Makefile
	tth < $< > $@ -w2 -e2

clean:
	rm -f $(PDF) *.aux *.log *.bib *.out *~ pc.mobi pc.html


