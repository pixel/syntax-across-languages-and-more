CAML = ocaml ocaml2 ocaml3bis
CPPs = c++ c++2 c++3
JAVA = java java2 java3
DYLAN = dylan dylan2 dylan3

ALL = $(CAML) $(CPPs) $(JAVA) $(DYLAN)

all: $(ALL)

$(CAML): %: %.listing
	ocamlc -o $@ -impl $^

$(CPPs): %: %.listing
	g++ -g -Wall -x c++ -o $@ $^

$(JAVA): %: %.listing
	gcj --main=$@ -g -Wall -x java -o $@ $^
#	javac $^
#	java $@


$(DYLAN): %: %.listing
	mindycomp -l$@-test -o $@-test-exports.dbc $@-exports.listing
	mindycomp -l$@-test -o $@-test.dbc         $@.listing
	mindy -f $@-test-exports.dbc -f $@-test.dbc

clean:
	rm -f *~ *.dbc *.class *.cmo *.cmi $(ALL)
