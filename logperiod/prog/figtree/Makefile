CC = gcc -Wall -O0 -g3 -pedantic
LM = -lm
# optimized compiling
CX = icc -Wall -O3 -g -march=native -vec_report0
LMX =
#CX = gcc -Wall -O3 -g -march=native -mfpmath=sse -funroll-all-loops
#LMX = -lm
deps = Makefile
bin = $(patsubst %.c,%, $(wildcard *.c))
bin_d = $(patsubst %.c,%_d, $(wildcard *.c))

all: logtree.png cubtree.png

$(bin) : % : %.c $(deps)
	$(CX) -o $@ $< $(LMX)

$(bin_d) : %_d : %.c $(deps)
	$(CC) -o $@ $< $(LM)

logtree.dat: figtree
	./figtree > $@

cubtree.dat: figtree
	./figtree -c > $@

%.ps: %.gp %.dat $(deps)
	gpload $<

%.pdf: %.ps $(deps)
	epstopdf $<

%.png: %.pdf $(deps)
	convert -rotate 90 $< $@
	cp $@ ../../img/

clean:
	rm -f *~ *.out MTSEED *.dat $(bin) $(bin_d) *.tree *.dat *.pdf *.ps *.png
	rstrip.py -Rv

.PHONY: clean all

