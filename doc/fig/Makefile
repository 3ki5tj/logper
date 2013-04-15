
PS = $(patsubst %.gp,%.ps, $(wildcard *.gp))
PDF = $(patsubst %.gp,%.pdf, $(wildcard *.gp))
PNG = $(patsubst %.gp,%.png, $(wildcard *.gp))

all: $(PDF) $(PNG)

$(PNG) : %.png : %.pdf
	convert $< $@

$(PDF) : %.pdf : %.ps
	epstopdf $<

$(PS) : %.ps : %.gp
	gpload $<

clean:
	rm -f $(PDF) $(PS) $(PNG) *~

