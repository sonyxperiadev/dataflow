SOURCES=$(shell find *.flow)
DFD_TARGETS=$(SOURCES:%.flow=%.dfd.png)
SEQ_TARGETS=$(SOURCES:%.flow=%.seq.png)
DATAFLOW=../dist/build/DataFlow/dataflow
PLANTUML=/tmp/dataflow-build/plantuml.jar

$(DATAFLOW):
	cd .. && cabal build

$(PLANTUML):
	@mkdir -p /tmp/dataflow-build
	@wget -qO $(PLANTUML) "http://downloads.sourceforge.net/project/plantuml/plantuml.jar?r=http%3A%2F%2Fsourceforge.net%2Fprojects%2Fplantuml%2F%3Fsource%3Dtyp_redirect&ts=1428479663&use_mirror=freefr"

%.dfd.png: %.flow $(DATAFLOW)
	@echo "$< -> $@"
	@$(DATAFLOW) dfd $< | dot -Tpng > $@

%.seq.png: %.flow $(DATAFLOW) $(PLANTUML)
	@echo "$< -> $@"
	@$(DATAFLOW) seq $< | java -Djava.awt.headless=true -jar $(PLANTUML) -tpng -pipe > $@

png: $(DFD_TARGETS) $(SEQ_TARGETS)

clean:
	rm -f $(DFD_TARGETS) $(SEQ_TARGETS)

all: png
