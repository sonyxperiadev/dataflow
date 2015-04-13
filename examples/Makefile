SOURCES=$(shell find *.hs)
TARGETS=$(SOURCES:%.hs=%.svg)

%.svg: %.hs
	@echo "$< -> $@"
	@runhaskell -i../src $< | dot -Tsvg -Efontname=Arial -Nfontname=Arial -Gfontname=Arial > $@

svg: $(TARGETS)

clean:
	rm -f $(TARGETS)

all: svg
