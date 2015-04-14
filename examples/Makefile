SOURCES=$(shell find *.flow)
TARGETS=$(SOURCES:%.flow=%.png)
DATAFLOW=../dist/build/DataFlow/dataflow

$(DATAFLOW):
	cd .. && cabal build

%.png: %.flow $(DATAFLOW)
	@echo "$< -> $@"
	@$(DATAFLOW) dfd $< | dot -Tpng > $@

png: $(TARGETS)

clean:
	rm -f $(TARGETS)

all: png
