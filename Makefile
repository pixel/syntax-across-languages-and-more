FROMM4FILES = index.html diagram.html
FROMPERLFILES = syntax-across-languages.html concepts-history.html
DIAGRAMFILES = $(foreach i, diagram diagram-light, $(i).png $(i)-thumbnail.png $(i).ps.gz $(i).pdf)
FILES = $(DIAGRAMFILES) $(FROMM4FILES) $(FROMPERLFILES)
DIRS = various usenet-traffic-ranking scripting-language

all: $(FILES) dirs

dirs:
	for i in $(DIRS); do $(MAKE) -C $$i; done

%.gz: %
	gzip -9c $< > $@

%-thumbnail.png: %.png
	convert -size 250x250 $< -resize 250x250 $@

diagram-light.png: %.png: %.dot
	dot -Tpng -Gsize="10,10" -Nfontname=Vera -Ncolor='#e0ffff' -Nstyle=filled $< > $@

%.png: %.dot
	dot -Tpng -Gsize="30,17" -Nfontname=Vera -Ncolor='#e0ffff' -Nstyle=filled $< > $@

%.pdf: %.dot
	dot -Tps2 -Gsize="10.5,7.8" -Nfontname=Helvetica -Nheight=0.9 -Ncolor='#e0ffff' -Nstyle=filled $< | ps2pdf - > $@

%.ps: %.dot
	dot -Tps -Gsize="16,9.8" -Nfontname=Helvetica -Nheight=0.9 -Gpage="8.2,11" -Grotate=90 $< > $@

%.svg: %.dot
	dot -Tsvg -Gsize="10,10" $< > $@

syntax-across-languages.html : %: %.pl
	rm -f $@
	perl $<
	chmod a-w $@

concepts-history.html: %: %.pl
	rm -f $@
	perl $< > $@
	chmod a-w $@

$(FROMM4FILES) : %: %.m4 ../mirrors.m4
	rm -f $@
	m4 $< > $@
	chmod a-w $@

#diagram.png: diagram.vcg
#	 rm -f /tmp/t.ppm
#	 xvcg -silent -ppmoutput /tmp/t.ppm -xdpi 600 -ydpi 50 $<
#	 convert /tmp/t.ppm $@
#	 rm -f /tmp/t.ppm

#diagram.ps: diagram.vcg
#	rm -f $@
#	xvcg -silent -landscape -split 9 -psoutput $@ $<

diagram.letter.ps: diagram.vcg
	rm -f $@
	xvcg -silent -paper letter -landscape -split 9 -psoutput $@ $<

diagram.vcg: language-study.list
	perl language-study.list2vcg $< > $@

diagram.dot: language-study.list
	BLOATED=1 perl language-study.list2dot $< > $@

diagram-light.dot: language-study.list
	perl language-study.list2dot $< > $@

clean: 
	rm -rf diagram.vcg $(FILES) syntax-across-languages syntax-across-languages-per-language diagram.dot diagram.ps diagram.letter.ps

# 