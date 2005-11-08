FROMPERLFILES = index.html
FILES = index_pl.txt $(FROMPERLFILES)

all: $(FILES)


$(FROMPERLFILES): %: %.pl
	rm -f $@
	perl $< > $@
	chmod a-w $@

index_pl.txt: index.html.pl
	rm -f $@
	cat $< > $@
	chmod a-w $@

clean: 
	rm -rf *~ *.listing $(FILES)
