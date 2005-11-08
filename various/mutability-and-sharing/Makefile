CAML = ocaml
CPPs = c++
JAVA = java
CSHARP = csharp.exe
DYLAN = 

ALL = $(CAML) $(CPPs) $(JAVA) $(CSHARP) $(DYLAN)

all: $(ALL)

$(CAML): %: %.listing
	ocamlc -o $@ -impl $^

$(CPPs): %: %.listing
	g++ -g -Wall -x c++ -o $@ $^

$(JAVA): %: %.listing
	gcj --main=$@ -g -Wall -x java -o $@ $^
#	javac $^
#	java $@

$(CSHARP): %.exe: %.listing
	mcs $^


$(DYLAN): %: %.listing
	mindycomp -l$@-test -o $@-test-exports.dbc $@-exports.listing
	mindycomp -l$@-test -o $@-test.dbc         $@.listing
	mindy -f $@-test-exports.dbc -f $@-test.dbc

clean:
	rm -f *~ *.dbc *.class *.cmo *.cmi $(ALL)
