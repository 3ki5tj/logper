
ijbc = IJBC-D-12-00312R1

#PDF = $(patsubst %.tex,%.pdf, $(wildcard *.tex))
PDF = pc.pdf pc-ijbc.pdf p3p.pdf $(ijbc).pdf
#figpng = $(wildcard *.png)
figpng = cobweb.png cubbulb.png halfcycle.png logbulb.png
figpdf = $(patsubst %.png,%.pdf, $(figpng))

all: $(PDF) p3p.html

$(PDF) : %.pdf : %.tex Makefile $(figpdf)
	pdflatex $<

%.mobi: %.html Makefile
	kindlegen -c1 -verbose $<

$(ijbc).tex: pc-ijbc.tex
	python rmcmt.py

# we force the PDF to be made before HTML
# -i to make mathematical symbols italic
# tth needs to know the base file name, which is furnished by the `stem' as -L$*
# -w2 to write XHTML header
%.html: %.tex %.pdf Makefile $(figpng)
	tth -a -i -w2 -e2 -L$* -w2 -e2 < $< > $@

usb::
	$(MAKE) -C .. $@

clean:
	rm -f $(PDF) *.aux *.log *.bib *.out *~ pc.mobi pc.html
	rstrip.py -Rv

ijbczip: $(ijbc).pdf
	zip $(ijbc).zip $(ijbc).pdf $(ijbc).tex Fig1.pdf copyright.pdf
