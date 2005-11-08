CAML = ocaml ocaml2 ocaml3 ocaml5
CPPs = c++ c++2 c++3 c++4

ALL = $(CAML) $(CPPs)

all: $(ALL)

$(CAML): %: %.listing
	ocamlc -o $@ -impl $^

$(CPPs): %: %.listing
	g++ -g -Wall -x c++ -o $@ $^

clean:
	rm -f *~ *.cmo *.cmi $(ALL)
