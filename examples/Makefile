SOURCES=$(shell find *.flow)
TARGETS=$(SOURCES:%.flow=%.svg)
DATAFLOW=../dist/build/DataFlow/dataflow

$(DATAFLOW):
	cd .. && cabal build

%.svg: %.flow $(DATAFLOW)
	@echo "$< -> $@"
	@$(DATAFLOW) dfd $< | dot -Tsvg > $@

svg: $(TARGETS)

clean:
	rm -f $(TARGETS)

all: svg
