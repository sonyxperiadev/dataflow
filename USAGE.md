# DataFlow Usage

The following declarations is supported by DataFlow.

<!--- Not Ruby code, but use Ruby code highlighter for .flow code -->

```ruby
boundary "Title" { ... }
io identifier "Title"
function identifier "Title"
database identifier "Title"
identifier -> identifier "Operation" "Data Description"
```

These are used inside a `diagram { ... }`.

## Example

```ruby
diagram {
  title = ""

  boundary {
    title = "Browser"

    function client {
      title = "Client"
    }
  }

  boundary "Amazon AWS" {
    function server {
      title = "Web Server"
    }
    database logs {
      title = "Logs"
    }
  }
  io analytics {
    title = "Google Analytics"
  }

  client -> server {
    action = "Request /"
  }
  server -> logs {
    action = "Log"
    data = "User IP"
    description =
      |A multiline
      |description.
  }
  server -> client {
    action = "Response"
    data = "User Profile"
  }
  analytics <- client {
    action = "Log"
    data = "Page Navigation"
  }
]
```

## DFD

![DFD Legend](examples/legend.dfd.png)

To use the *DFD* output you need [Graphviz](http://www.graphviz.org/) installed.

```bash
dataflow dfd webapp.flow | dot -Tpng > webapp.png
```
### Output

![DFD Output](examples/webapp.dfd.png)

## Sequence Diagram

![Sequence Diagram Legend](examples/legend.seq.png)

You can use [PlantUML](http://plantuml.sourceforge.net/) to generate a sequence
diagram.

```bash
dataflow seq webapp.flow | java -Djava.awt.headless=true -jar plantuml.jar -tpng -pipe > webapp.png
```

### Output

![Sequence Diagram Output](examples/webapp.seq.png)

## Makefile Example

The following Makefile finds `.flow` sources in `src` and generates DFDs, in
SVG format, in `dist`.

```make
SOURCES=$(shell find src/*.flow)
TARGETS=$(SOURCES:src/%.flow=dist/%.dfd.svg)

K := $(if $(shell which dataflow),,$(error "No dataflow executable in PATH. See https://github.com/SonyMobile/dataflow for install instructions)))"))

dist/%.dfd.svg: src/%.flow
	@dataflow dfd $< | dot -Tsvg > $@

dfd: $(TARGETS)

clean:
  rm -f $(TARGETS)
```
