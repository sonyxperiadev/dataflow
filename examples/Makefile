SOURCES=$(shell find *.hs)
TARGETS=$(SOURCES:%.hs=%.svg)

%.svg: %.hs
	@echo "$< -> $@"
	@runhaskell -i../src $< | dot -Efontname=Helvetica -Nfontname=Helvetica -Gfontname=Helvetica -Tsvg > $@

svg: $(TARGETS)

clean:
	rm $(TARGETS)

all: svg
