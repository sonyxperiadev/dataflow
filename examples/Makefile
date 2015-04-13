SOURCES=$(shell find *.hs)
TARGETS=$(SOURCES:%.hs=%.svg)

%.svg: %.hs
	@echo "$< -> $@"
	@runhaskell -i../src $< | dot -Efontname=Arial -Nfontname=Arial -Gfontname=Arial -Tsvg > $@

svg: $(TARGETS)

clean:
	rm $(TARGETS)

all: svg
