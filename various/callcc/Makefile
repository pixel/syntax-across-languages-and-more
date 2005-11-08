LISTINGS = $(wildcard *.listing)
LISTINGS_HTML = $(LISTINGS:%.listing=%.html)

all: $(LISTINGS_HTML)

%.html: %.listing
	emacs2html $^
	./add_description.pl $@

clean:
	rm -f *~ $(LISTINGS_HTML)
