LANGS1 = java c++ c perl basic python javascript tcl ruby pascal
LANGS2 = ruby lisp ada smalltalk cobol
LANGS3 = fortran forth scheme eiffel prolog ml
LANGS4 = object functional

L = langs1 langs2 langs3 langs4

FILES = $(foreach l, $L, $l.png $l-small.png $l.eps)

all: $(FILES)

langs1:
	./plot_it.pl $(LANGS1)

langs2:
	./plot_it.pl $(LANGS2)

langs3:
	./plot_it.pl $(LANGS3)

langs4:
	./plot_it.pl $(LANGS4)

langs1.png:
	./plot_it.pl -t png $(LANGS1) > $@

langs2.png:
	./plot_it.pl -t png $(LANGS2) > $@

langs3.png:
	./plot_it.pl -t png $(LANGS3) > $@

langs4.png:
	./plot_it.pl -t png $(LANGS4) > $@

langs1-small.png:
	./plot_it.pl -t small-png $(LANGS1) > $@

langs2-small.png:
	./plot_it.pl -t small-png $(LANGS2) > $@

langs3-small.png:
	./plot_it.pl -t small-png $(LANGS3) > $@

langs4-small.png:
	./plot_it.pl -t small-png $(LANGS4) > $@

langs1.eps:
	./plot_it.pl -t eps $(LANGS1) > $@

langs2.eps:
	./plot_it.pl -t eps $(LANGS2) > $@

langs3.eps:
	./plot_it.pl -t eps $(LANGS3) > $@

langs4.eps:
	./plot_it.pl -t eps $(LANGS4) > $@


clean:
	rm -f *~ data/*~ data/.log.* .tmp .gnuplot_it $(FILES)
