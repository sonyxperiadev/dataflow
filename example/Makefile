SOURCES=$(shell find *.hs)
TARGETS=$(SOURCES:%.hs=%.svg)

%.svg: %.hs
	@echo "$< -> $@"
	@runhaskell $< | dot -Tsvg > $@

svg: $(TARGETS)

clean:
	rm $(TARGETS)

all: svg
