
#PDF = $(patsubst %.tex,%.pdf, $(wildcard *.tex))
PDF = pc.pdf pc-ijbc.pdf p3p.pdf

all: $(PDF) p3p.html

$(PDF) : %.pdf : %.tex  Makefile
	pdflatex $<

pc.mobi: pc.html Makefile
	kindlegen -c1 -verbose $<

%.html: %.tex Makefile
	tth < $< > $@ -w2 -e2 -i

# -i to make mathematically symbols italic

clean:
	rm -f $(PDF) *.aux *.log *.bib *.out *~ pc.mobi pc.html


